{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Buchhaltung.Add (add)
where

import           Buchhaltung.Ask
import           Buchhaltung.Common
import           Buchhaltung.ZipEdit2 as ZE
import           Buchhaltung.Zipper
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Data.Char
import           Data.Decimal
import           Data.Either
import           Data.Foldable
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Data.List
import qualified Data.List.NonEmpty as E
import           Data.List.Utils (replace)
import qualified Data.ListLike as L
import qualified Data.ListLike.String as L
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid ((<>))
import qualified Data.Monoid as Mo
import           Data.Ord
import qualified Data.Set as S
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Formatting (sformat, (%), (%.))
import qualified Formatting as F
import qualified Formatting.ShortFormatters as F
import           Hledger hiding (at)
import           Hledger.Cli.CliOptions
import           Safe
import qualified Text.Megaparsec as MP hiding (State)
import qualified Text.Megaparsec.Text as MP hiding (State)
import           Text.Parsec.Char
import           Text.Parsec.Combinator (optionMaybe,eof,many1)
import           Text.Parsec.Prim (runParser,parse,try,runP)
import           Text.Parsec.String
import           Text.Printf
import           Text.Read (readEither)

-- DONE: clear an entry without entering a new one
-- TODO: percentage entry with memory
-- TODO: other ways to find uncleared transactions (e.g. by account)
-- DONE cancel unsuccesful search (and do ot force start of transaction entry)
-- TODO: do not propose transfer accounts?
-- TODO: change (existing) titles to include account
-- TODO: insert assertions? (for geldbeutetl or automatic for hbci)
-- TODO: check assertions (gegenseitige schulden)
-- TODO: do not jump to next suggestion (only after half)
-- TODO: betrag vorschlagen mit history? oder siehen nächstes
-- TODO: möglichenkeiten der eingabe von beträgen für beide parteien gleichzeitig
-- TODO: keine kontonamen in falscher reihenfolge vorschlagen

-- DONE in 8fbbc34: BUG: discard stil marks as cleared

-- * Types

type AddT' env m = RWST (FullOptions env) () Journal (ErrorT m)


-- | Monad Transformer used to describe the 'add' program
--
-- The 'add' specific environment, that can contain a partner user

type AddOptions = FullOptions [Partner]

type AddT m = AddT' [Partner] m


data Partner = Partner
  { pUser :: User
  , partnerAccount :: AccountName
    -- ^ partner's receivable/payable account in the user's ledger
  , userAccount :: AccountName
    -- ^ user's receivable/payable account in the partner's ledger
  , partnerLedger :: FilePath
    -- ^ partner's ledger
  }
             deriving (Show, Eq)

data Remote = Remote { account :: AccountName
                     , ledger :: FilePath}  
  
-- | Extract partner information from the env and throw errors if there are any
-- readPartner :: (MonadReader AddOptions m, MonadError Msg m)
--             => (Partner -> a) -> m (Maybe a)
-- readPartner f = reader $ fmap f . oEnv
  
-- -- | Extract partner user
partners
  :: (MonadReader AddOptions m, MonadError Msg m) => m [Partner]
partners = reader oEnv
  
-- * Entry point

add :: AddT' [Username] IO ()
add = do
  partner <- mapM (toPartner <=< lookupUser) =<< reader oEnv
  liftIO . putStr =<< hello
  withRWST (\r s -> (r{oEnv = partner}, s)) $ do
    forever mainLoop

-- | Convert given 'Username' to 'Partner'.
toPartner :: Monad m => User -> AddT' env m Partner
toPartner part = do
  us <- user
  Partner part
    <$> receivablePayable True  part
    <*> receivablePayable False part
    <*> maybeThrow msg ($ name part) return (addedByOthers $ ledgers part)
  where msg = "ledgers.addedByOthers not configured for '"%F.sh%"'" 
                 

-- | Welcome message
hello :: Monad m => AddT' env m String
hello = do
  un <- readUser name
  j <- get
  return $ intercalate "\n\n" [
    "Hi "<> fshow un <> "!"
    ,"Use readline keys to edit, use tab key to complete account names."
    ,"A code (in parentheses) may be entered following transaction dates."
    ,"A comment may be entered following descriptions or amounts."
    , show ( length $ jtxns j) ++ " Transactions found"
    ] 
  

-- | main user interaction loop
mainLoop :: AddT IO ()
mainLoop = do
  liftIO $ putStr "\n\nStarting new transaction...\n"
  (iAm, match) <- sugTrans
  let ask j = do iDa <- askDate Nothing
                 iDe <- askDescription Nothing
                 iAc <- myAskAccount j Nothing (Just "learn") (Left "Enter source account" )
                 return (nulltransaction{tdate=fst iDa, tcode=snd iDa
                                        ,tdescription=iDe}
                        ,iAc,iAm)
      useMatch t = return ( nulltransaction{tdate=tdate t,
                                            tdescription=tdescription t}
                          ,paccount p2, ("",negate $ pamount p2))
        where _:(p2:_) =  tpostings t
        --                   ,paccount p3, ("",negate $ pamount p3))
        -- where p3 = fromMaybe (pos!!2) $ asum $
        --            (\x -> return x <$> extractSource "HBCI" (pcomment x)) <$> pos
        --       pos = tpostings t
  -- PROBLEM1: it's not HBCI, instead sugTrans and clearSecondPosting
  -- emphazises on the "second" posting
  (transa,iAc,iAm2) <- maybe (get >>= liftIO . ask) useMatch match
  suggs <- suggestedPostings iAc $ Just iAm2
  result <- edit myEd transa suggs moveToNextEmpty
  saveAndClear match (isJust result) =<< finishTransaction True result

-- | Saves transaction into the designated ledgers files of each user,
-- and clears the transaction tht was matched (conditional on a 'Bool'
-- argument)
saveAndClear :: Maybe Transaction
             -- ^ matching transaction for clearing
             -> Bool -- ^ clear the matching transactions
             ->   (Maybe Transaction, [(Partner, Transaction)])
             -- ^ transactioons to be saved: ((user's,other's),
             -> AddT IO ()
saveAndClear match clearIt (userT, partnerTS)  = do
  j <- get
  let saveMine Nothing = clear j
      saveMine (Just res) = do
        -- clear the first posting, of the new transaction, if there was a
        -- match -- ReferenceA
        let newt = (if isJust match then clearNthPosting 0 else id) res
        -- add it to the user's journal (+ file)                
        file <- readLedger addedByThisUser
        j' <- myJournalAddTransaction file [newt]
        infoNewTx =<< user
        -- clear the matching transactions second posting in the file
        clear j'
      clear j = if clearIt && isJust match then
                  saveChanges $ changeTransaction $ clearMatch $ fromJust match
                else return j
      savePartners (partner, tx) = do
        myJournalAddTransaction (partnerLedger partner) [tx]
        infoNewTx $ pUser partner
  j' <- saveMine userT
  mapM savePartners partnerTS
  put j'


-- | maybe this is not enough (see comments below)
clearMatch t = [Just (t, clearNthPosting 1 t)]
  -- compare comment, because e..g amounts can have different formats,
  -- because on is read from clearF and the other from via import in
  -- main ledger
              
  -- alternative: also import matches from clearF. or use a database
  -- with IDs :-)

  -- TODO ^ 
             
-- | clears the second posting of a given transaction in a journal
clearSecondPosting :: Maybe Transaction -> Journal
                      -> (Journal,Integer)
                      -- ^ return number of changes and new journal
clearSecondPosting Nothing  = flip (,) 0
clearSecondPosting (Just t1) = countUpdates (jTrans . traverse) clear
  where clear t2 | t1 == t2 = Just $ clearNthPosting 1 t2
                 | True     = Nothing
        

  
infoNewTx = liftIO . L.putStr .
  sformat ("\nNew transaction created for '" %F.sh% "'\n") . name

-- | Split 'EditablePosting's in User's Postings and (Partner,
-- Posting)
split :: [EditablePosting] -> ([Posting],
                               [(Partner, E.NonEmpty Posting, MixedAmount)])
split = second (fmap h . E.groupBy (on (==) fst))
        . partitionEithers . mapMaybe f
  where
    f :: EditablePosting -> Maybe (Either Posting (Partner, Posting))
    f x = either (const Left) ((Right .) . (,)) (present $ epUser x)
          <$> epPosting x
    h :: E.NonEmpty (Partner, Posting)
      -> (Partner, E.NonEmpty Posting, MixedAmount)
    h x = (fst $ E.head x, snd <$> x, sum $ pamount . snd <$> x)

-- | generate the main and possibly the other users' transactions
finishTransaction :: (MonadIO m, MonadReader AddOptions m)
                  =>  Bool
                     -- ^ require balanced transactions
                  -> Maybe (Transaction, [EditablePosting])
                  -- ^ the transaction and postings to be combined
                  -> m (Maybe Transaction, [(Partner, Transaction)])
                  -- ^ (user's, partners') transactions
finishTransaction _ Nothing = return (Nothing, [])
finishTransaction check (Just (tr,postings)) = do
  time <- liftIO getCurrentTime
  us <- user
  let
    (userP, partnerPS) = split postings

    userT = if null x then Nothing else Just $ toTP x
      where x = userP ++ concatMap userTransfer partnerPS
    userTransfer (partner, ps, sum) =
      nullP (partnerAccount partner) sum

    partnerT (partner, ps, sum) = (,) partner $
      toTP $ nullP (userAccount partner) (negate sum)
      ++ E.toList ps
    
    toTP ps = (if check then either err id . balanceTransaction Nothing
              else id)
              tr {tpostings = increasePrec <$> ps ,tcomment = comment}

    nullP acc am = if isReallyZeroMixedAmount am then []
                   else [nullposting{paccount = acc
                                    ,pamount = am}]

    comment = sformat ("Entered on "%F.sh%" by 'buchhaltung' user "%F.sh)
      (iso8601 time) $ name us
    err = (error.("shit, it should be balanced, check source code\n\n"++))
    increasePrec p = p{pamount = setMixedAmountPrecision maxprecisionwithpoint
                        $ pamount p}
  return $ (userT, partnerT <$> partnerPS)
        
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%TZ"
        
-- | add transaction to ledger file    
myJournalAddTransaction :: FilePath -> [Transaction] -> AddT IO Journal
myJournalAddTransaction relative trans = do
  file <- absolute relative
  liftIO $ ensureJournalFileExists file -- creates empty journal
  j <- get
  liftIO $ L.appendFile file
    $ "\n" <> intercalateL "\n\n" trans' <> "\n"
  return j{jtxns=jtxns j ++ trans }
  where trans' = L.dropWhileEnd (=='\n') . fshow <$> trans :: [T.Text]


              
clearNthPosting :: Int -> Transaction -> Transaction
clearNthPosting n t = t{tpostings=p'}
  where p' = modifyNth (\x -> x{pstatus=Cleared}) n $ tpostings t

                              
-- * Transaction suggestions

-- | Find a transactions matching the entered amount
sugTrans :: AddT IO ((Comment, MixedAmount), Maybe Transaction)
sugTrans = --error $ unlines $ show <$> Set.elems s
  do
    r@(_, iAm) <- second negate <$> askAmount (Just $ mixed' nullamt)
      "Enter amount (zero for any transaction)" Nothing
    
    accs <- S.fromList . HM.elems <$> readUser (fromBankAccounts . bankAccounts)
    user <- readUser id
    let
      f user accs t@Transaction{tpostings=p1:(p2:_)} =
        -- the first posting's account is part of the accounts
        -- automaticcaly handled by csv2ledger
        (paccount p1 `S.member` accs)
        -- the first amount of the first posting matched the entered
        -- amount in absolute values
        && (on (==) (abs.aquantity.head.amounts) iAm ( pamount p1 )
            -- or the amount is zero    
            || mixed' nullamt == iAm
            -- or the amount occurs in the comment of the second
            || (comma.fshow.abs.aquantity.head.amounts $ iAm)
            `L.isInfixOf` pcomment p2 )
        -- the second posting is not cleared
        && not (pstatus p2 == Cleared)
        -- doch nicht (siehe notiz vom [2013-06-02 Sun]):
        -- no transaction exists offsetting the second
        -- posting (e.g. for transactions entered, before they were
        -- paid)
        -- && fromMaybe True ( flip Set.notMember s <$>
        --     toMyP t p2{pamount=negate $ pamount p2})
        
        -- ignore certain accounts
        && (not $ isIgnored user $ paccount p2)
      f _ _ _ = False
        -- s :: S.Set MyPosting
        -- s = error "doch benutzt?" -- Set.fromList $ toMyPs =<< jtxns j
      comma = T.replace "." ","
      selectMatch :: [Transaction]
                  -> AddT IO ((Comment, MixedAmount), Maybe Transaction)
      selectMatch m = g =<< (liftIO $ choose $ show <$> m')
        where m' = take 20 $ sortBy (flip $ comparing tdate) m
              g Manual = return (r, Nothing)
              g (Choose i) = return $ (r, Just $ atNote "selectMatch" m' i)
              g Reenter = sugTrans
    selectMatch =<< gets (filter (f user accs) . jtxns)

-- data MyPosting = MyP {mypDay::Day, mypAcc::AccountName,mypAmt::Amount}
--                  deriving (Show)

-- toMyPs t = catMaybes [ toMyP t p  | p <- tpostings t ]

-- toMyP t p@Posting{pstatus=Uncleared} =
--   MyP (tdate t) (paccount p) <$> listToMaybe (amounts $ pamount p)
--                                                          --take only first amount
-- toMyP _ _ = Nothing


-- compareMyPs = comparing (acommodity.mypAmt) && (aquantity.mypAmt) && mypDay && mypAcc
--     where (&&) a b = a `Mo.mappend` comparing b
-- instance Ord MyPosting where
--   compare = compareMyPs
-- instance Eq MyPosting where
--   (==) =  (EQ==).:compareMyPs
-- (.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
-- (.:) = (.) . (.)
         
data Choice = Reenter | Manual | Choose Int
  
-- | user input: choose one from a list of choices
choose :: [String] -- ^ choices
       -> IO Choice
choose m = do
  putStr $ unlines $ reverse $
    zipWith pr [1..] m
  putStr $ unlines
    [if null m then ""
      else printf "{number}: use one of the above %d transactions" len
    ,"'m': enter transaction manually"
    ,"'r': enter new amount to find existing transations"]
  either ((>> choose m) . print) return
    =<< parse (menup len)"menu parser" <$> getLine 
  where pr n t = show n ++ ")\n" ++ t
        len = length m

menup :: Int -> Parser Choice
menup limit = stripP $
  try (try (char 'r' >> return Reenter)
        <|> (char 'm' >> return Manual))
  <|> do int <-pred . read <$> many1 digit
         if int < limit then return $ Choose int
           else fail "number too high"

stripP p = do r <- spaces >> p
              spaces >> eof >> return r
  
                             
-- * Main editing loop

-- | combines everything into an 'EditorConf'
myEd :: EditorConf (AddT IO) EditablePosting Transaction
myEd =
  EC { getchar = Just myGetchar
     , ZE.display = dis
     , ecPrompt = const mainPrompt
     , actions = [
         ('c', ModifyAll clearTrans <> Done quiet
           ?? "Clear transaction")
         , ('n', ModifyAllM (addNewPosting False)   ?? "Add new posting")
         , ('N', ModifyAllM (addNewPosting True)
             ?? "Add new posting for next user")
         , ('r', ModifyAll (assignOpenBalance 1.0)
             ?? "Assign open balance to account")
         , ('h', ModifyAll (assignOpenBalance 2.0)
             ?? "Assign half of open balance to account")
         , ('x', Modify removeAmount   ?? "Remove current amount")
         , ('t', ModifyStateM editDescription  ?? "Edit title")
         , ('d', ModifyStateM editDate  ?? "Edit date")
           -- http://www.ledger-cli.org/3.0/doc/ledger3.html#Effective-Dates
           -- , ('D', ModifyStateM (editDate j)  ?? "Edit effective date")
         , ('e', ModifyAllM editCurAmount ?? "Edit current amount")
         , ('+', ModifyAllM (modifyCurAmount (+) False) ?? "Add to amount")
         , ('j', Fwd    ?? "Move forward one item.")
         , ('k', Back   ?? "Move backward one item.")
         , ('Q', Cancel ?? "Discard entry")
         , ('s', Done checkDone  ?? "Save entry")
         , ('c', Done checkDone  ?? "Save entry")
         , ('u', Modify nextNotFirst  ?? "Next user")
         , ('m', ModifyAll (assignOpenBalance (119/19)) ?? "Fill with MWSt")
         , ('p', ModifyAllM
                 ((<$> liftIO askPercent) . flip assignOpenBalance)
                 ?? "Ask for percentage")
         ] ++ digitActions
     }
  where digitActions =
          [ (intToDigit n, ModifyAll (jumpTo n) ??
                  ("Edit account"++[intToDigit n])) | n <- [0..9] ]
        dis LS{userSt=tr,ctx=z} = do
          j <- get
          return $ L.replicate 60 '~' <> "\n\n"
            <> fromString (showTransaction tr)
            -- <> fshow (jparsedefaultcommodity j, jcommodities j, jinferredcommodities j)
            <> showEditablePosting z

mainPrompt :: (L.ListLike m item, IsString m) => m
mainPrompt =
  "\n" <> intercalateL "\n" ( intercalateL "   " <$> f) <> "\n"
  where f =
          [["[r]est ","[j]next","[u]ser ","[+]add","[t]itle","[?]Help  ","[s]ave    ","[p]%"]
          ,["[h]alf ","[k]prev","[n/N]ew","[e]dit","[d]ate ","[x]remove","[Q]discard","[m]WSt"]
          ,["       ","       ","       ","      ","       ","         ","[c]lear   ","      "]]

quiet :: Monad m => a -> m (Maybe a)
quiet = return . Just

-- | clear all postings
clearTrans :: Zipper EditablePosting -> Zipper EditablePosting
clearTrans = differentiate . fmap f . integrate'
  where f x  = x{epPosting=Nothing}

-- | change user of current posting, but not for the first posting
-- ReferenceA
nextNotFirst :: EditablePosting -> EditablePosting
nextNotFirst s | epNumber s > 0 = next s
               | otherwise = s
         
-- | Try to balance the transactions and present the final
-- transactions
checkDone :: LState EditablePosting Transaction ->
  AddT IO (Maybe  (LState EditablePosting Transaction))
checkDone st@LS{userSt = trans, ctx = postings} = do
  user <- user
  (userT, partnerT) <- finishTransaction False $ Just (trans, integrate postings)
  let
    txs :: [(User, Either String Transaction)]
    txs = second (balanceTransaction Nothing)
        <$> maybeToList ((,) user <$> userT) ++ fmap (first pUser) partnerT
  res <- liftIO $ mapM g txs
  return$ if and res then Just st else Nothing
    where g (u,tx) = either (showR "ERROR" False u)
                     (showR "Balanced Transaction" True u . show) tx
          showR title ret user msg  = do
            L.putStrLn $ sformat
              ("#######  "%F.sh%":  "% (F.center 20 ' '%.F.s) %"   #######\n\n"%F.s%"\n")
              (name user) title msg
            return ret

-- * Asking (with completion)

askDescription :: Maybe T.Text -- ^ default
                  -> IO T.Text
askDescription = editLoop Right d Nothing Nothing (Left d)
  where d = "Title" :: IsString a => a
        
-- | uses 'dateandcodep'
askDate :: Maybe Day -- ^ default
           -> IO (Day, T.Text)
askDate def = do
  today <- getCurrentDay
  let
    defday = fromMaybe today def
    -- defday = either (error . ("cannot parse default date:\n"++) . show)
    --          (fixSmartDate today) $ (parse smartdate "" . lowercase) $ showDate today
    extract :: T.Text -> Either String (Day, T.Text)
    extract input = ("Date parsing error "++).show +++ first
                    (fixSmartDate today)
                     $ MP.runParser dateandcodep "" input
  editLoop extract "Date" (Just ((defday,""), fshow defday))
    complList (Left "Date") $ fshow <$> def
  where complList = Just ["january"
                         , "february"
                         , "march"
                         , "april"
                         , "may"
                         , "june"
                         , "july"
                         , "august"
                         , "september"
                         , "october"
                         , "november"
                         , "december"
                         , "today"
                         , "yesterday"
                         , "tomorrow"
                         , "last"
                         , "this"
                         ,"next"]

-- | HLedger's 'smartdate' and code
dateandcodep :: MP.Parser (SmartDate, T.Text)
dateandcodep = do d <- smartdate
                  c <- optional codep
                  many spacenonewline
                  MP.eof
                  return (d, maybe "" T.pack c)

myAskAccount j = askAccount completionList
  where completionList = nub $ sort [ paccount p | t <- jtxns j
                                      , p <- tpostings t]

askAmount :: Maybe MixedAmount -- ^ default value, if "" is entered
             -> T.Text  -- ^ prompt
             -> Maybe T.Text -- ^ initial
             -> AddT IO (Comment, MixedAmount)
askAmount def pr init = do
  j <- get
  liftIO $ editLoop (extract j) "Amount" (def >>= useDef) Nothing (Left pr) init
  where
    extract :: Journal -> T.Text -> Either String (Comment, MixedAmount)
    extract j input = left show $ (,) cmt . mixed' <$> parseAmount j am
      
      where (am, cmt) =  textstrip *** (textstrip . L.dropWhile (==';')) <<< L.break (==';') $ input
              :: (T.Text, T.Text)
    useDef x = Just (("",x), showMixedAmount2 x)


-- -- | Parse and update an amount to equal its 'show' value
-- --
-- -- this is required to ensure, that the saved transaction equals the
-- -- edited and balanced transaction.
-- toShow :: Journal -> MixedAmount -> MixedAmount
-- toShow j (Mixed ams) = Mixed $ fmap g ams
--   where g = either (error.show) id . parseAmount j . fshow
  
parseAmount
  :: Journal
     -> T.Text -> Either (MP.ParseError Char MP.Dec) Amount
parseAmount j = parseWithState' j amountp

-- | (unused) overwrite upstream behavior to use defined or incurred
-- commodities
amountp2 = MP.try leftsymbolamountp
  MP.<|> MP.try rightsymbolamountp
  MP.<|> nosymbolamountp2

nosymbolamountp2 :: Monad m => JournalStateParser m Amount
nosymbolamountp2 = do
  (q,prec,mdec,mgrps) <- lift numberp
  p <- priceamountp
  -- apply the most recently seen default commodity and style to this commodityless amount
  defcs <- getDefaultCommodityAndStyle2 <$> get
  let (c,s) = case defcs of
        Just (defc,defs) -> (defc, defs{asprecision=max (asprecision defs) prec})
        Nothing          -> ("", amountstyle{asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps})
  return $ Amount c q p s
  MP.<?> "no-symbol amount"

getDefaultCommodityAndStyle2
  :: Journal
  -> Maybe (CommoditySymbol,AmountStyle)
getDefaultCommodityAndStyle2
  Journal{jparsedefaultcommodity=def
         ,jcommodities=comms
         ,jinferredcommodities=inferred} =
  let mm = listToMaybe . M.toList in
    asum
    [def
    ,traverse cformat =<< mm comms
    ,mm inferred
    ]


askPercent :: IO Decimal
askPercent = editLoop extr "percent"
             (Just ((,).prep <*> fshow $ def)) Nothing (Left "Percentage") Nothing
  where
    extr :: T.Text -> Either String Decimal
    extr s = show +++ prep $ readEither $ T.unpack s
    def = -1.5 :: Decimal
    prep = (100*).recip

-- * Posting and Suggestions

  
-- | Type holding suggested or temporary postings
data EditablePosting = EditablePosting { epPosting :: Maybe Posting
                             , epFreq    :: Maybe Int
                             , epAccount :: AccountName
                             , epNumber  :: Int
                             , epUser :: Zipper (Either User Partner)
                             -- ^ zipper of possible user
                             }
                           
type EditablePostings = Zipper EditablePosting

-- | construct an 'EditablePosting'
editablePosting account amt n = do
  ps <- partners
  user <- user
  return $ addPosting account amt EditablePosting
    { epAccount = account
    , epFreq=Nothing
    , epNumber=n
    , epPosting = Nothing 
    , epUser = differentiate $ E.cycle $ Left user E.:| (Right <$> ps)}
  
-- | generate and add new 'Posting' to 'EditablePosting'
addPosting
  :: AccountName
     -> Maybe (T.Text, MixedAmount) -> EditablePosting -> EditablePosting
addPosting _        Nothing s = s
addPosting account (Just (cmt, iam)) s = s{epPosting=Just nullposting{
  paccount=account,
  pcomment=cmt,
  pamount=iam}}

removeAmount :: EditablePosting -> EditablePosting
removeAmount ep = ep{epPosting=Nothing}
                  
-- | jump to a certain element in a zipper
jumpTo :: Int -> Zipper a -> Zipper a
jumpTo n z' = atNote "jumpTo" (iterate fwd . differentiate . integrate' $ z') n 
  
editDate :: LState a Transaction -> AddT IO (LState a Transaction)
editDate s@LS{userSt=t@Transaction{tdate=d}} = liftIO $ do
   iDa <- askDate (Just d)
   return s{userSt=t{tdate=fst iDa,tcode=snd iDa}}
                                                               
editDescription :: LState a Transaction -> AddT IO (LState a Transaction)
editDescription s@LS{userSt=t@Transaction{tdescription=d}} = liftIO $ do
   iDe <- askDescription (Just d)
   return s{userSt=t{tdescription=iDe}}
  
-- | edit the amount of the selected posting  
editCurAmount :: EditablePostings -> AddT IO EditablePostings
editCurAmount = modifyCurAmount (const id) True

-- | modfiy current amount by asking for a new amount, that is
-- combined with the old to get a new amount (e.g. with (+))
modifyCurAmount ::(MixedAmount -> MixedAmount -> MixedAmount)
               -- ^ oldAmout -> enteredAmount -> newAmount
               -> Bool  -- ^ Show old amound
               -> EditablePostings -> AddT IO EditablePostings
modifyCurAmount new showO z@LZ{past=(s@EditablePosting{epAccount=ac} E.:| ps)} = do
  iAm <- askAmount olda ("Amount for " <> ac) solda
  return $ moveToNextEmpty z{ past = addPosting ac (Just $ maybe id (second . new) olda iAm) s E.:| ps }
    where olda = pamount <$> epPosting s
          solda = if showO then showMixedAmount2 <$> olda else Nothing
          
  
-- | Hardcoded default number of suggested accounts
defNumSuggestedAccounts :: Int
defNumSuggestedAccounts = 20


-- | retrieve a number of suggested contra postings for a given
-- account, sort frequency of that contra account for the given
-- account.
-- 
-- duplicate each posting for both users, but only if the
-- other user's account is present in the suggestions.
suggestedPostings :: MonadIO m
                  => AccountName
                  -> Maybe (Comment,MixedAmount)
                  -> AddT m (E.NonEmpty EditablePosting)
suggestedPostings account am = do
  j <- get
  filterPref <- maybe id (\x -> filter $ not . L.isPrefixOf (x <> ":") . epAccount)
                <$> readUser accountPrefixOthers 
  nP <- succ . length <$> partners
  let 
    toEp l = editablePosting (head l) Nothing 0
      >>= \x -> return $ x{epFreq=Just $ length l}
    matches = concatMap tail $ filter filt $
              ((paccount<$>) . tpostings) <$> jtxns j
  sugaccounts <- sortBy (flip $ comparing epFreq)
                 <$> mapM toEp (group $ sort matches)
  let
    s = filterPref sugaccounts
    transformed :: [EditablePosting]
    transformed = if length sugaccounts == length s then s
                  else do x <- s
                          take nP $ iterate next x
  firstP <- editablePosting account am 0 -- ReferenceA
  num <- fromMaybe defNumSuggestedAccounts <$>
    readUser numSuggestedAccounts
  return $ (E.:|) firstP $ take (pred num) $ 
    zipWith (\n p -> p{epNumber=n}) [1..] transformed
  where
    filt x = account == head x && not ( null x )

-- | change posting's user to the next user
next :: EditablePosting -> EditablePosting
next s = s{epUser = fwd $ epUser s}

roundP :: EditablePosting -> EditablePosting
roundP p = p{epPosting = r1 <$> epPosting p}
  where r1 p = p{pamount = normalizeMixedAmountWith g $ pamount p}
        g a = roundTo (fromIntegral $ asprecision $ astyle a) $ aquantity a
        
                              
-- | assign the 'Transaction''s open balance to an empty 'EditablePosting'
assignOpenBalance :: Decimal -> EditablePostings -> EditablePostings
assignOpenBalance c old@LZ{past=(pr@EditablePosting{epAccount=sac} E.:| ps)} =
  moveToNextEmpty $ old{past= roundP (newp $ epPosting pr) E.:| ps}
  where balance = divideMixedAmount (totalBalance all) c
        all = integrate old
        newp Nothing = addPosting sac (Just ("", balance)) pr
        newp (Just op) = pr{epPosting = Just op{pamount= balance + pamount op }}

showEditablePosting :: EditablePostings -> T.Text
showEditablePosting LZ{past= pr E.:| ps ,future=fut} =
  renderTable (replicate 3 AlignCenter,replicate 3 AlignLeft,
               [ "Account",  "Amount", "Frequency"])
  [ let mark = if marked then "->" else "  " :: String
    in [
        sformat (F.d % "," %F.sh% " " %F.s% " " %F.st)
         (epNumber x) (either name (name.pUser) $ present $ epUser x) mark
         $ epAccount x
       ,maybe "" (showMixedAmount2 . pamount ) $ epPosting x
       ,maybe "" fshow $ epFreq x
       ] | (marked,x) <- postings ]
  $ Just ["Open Balance",balance,""]
  where balance = showMixedAmount2 $ totalBalance $ snd <$> postings
        postings = reverse ((,) True pr : mark ps) ++ mark fut
        mark = ((,) False <$>)

totalBalance :: [EditablePosting] -> MixedAmount
totalBalance = negate . sum . fmap pamount . mapMaybe epPosting
                                  
showMixedAmount2 :: MixedAmount -> T.Text
showMixedAmount2 = T.pack . showMixedAmountWithPrecision maxprecisionwithpoint
-- showMixedAmount2 = T.pack . showMixedAmountDebug 
  
-- | ask for new account (display old as default) and use existing
-- posting, if same account without a posting/amount already exists or
-- append.
addNewPosting :: Bool -- ^ for next partner
              -> EditablePostings -> AddT IO EditablePostings
addNewPosting forNext old' = do
  j <- get
  let postings = integrate old'
      nextP = (if forNext then fwd else id)
        $ epUser $ present old'
  account <- liftIO $ myAskAccount j
    (Just $ epAccount $ present old') Nothing $ Left "Account"
  new <- editablePosting account Nothing $ length postings
  let loop (ep:eps) done =
        if isNothing (epPosting ep)
           && epAccount ep == account
           && on (==) present nextP (epUser ep)
        then LZ (addPosting account Nothing ep E.:| done) eps
        else loop eps (ep:done)
      loop [] done = LZ (new E.:| done) []
  return $ modifyPresent (\x -> x{epUser = nextP}) $ loop postings []

  
-- | move the focus to the next empty posting  
moveToNextEmpty :: EditablePostings -> EditablePostings
moveToNextEmpty z = fromMaybe z $ find f zs
  where zs =  take (length postings) $ iterate wrapFwd z
        postings = integrate' z
        f = isNothing . epPosting . E.head . past 
        wrapFwd x = if null $ future x then differentiate postings
                    else fwd x


-- * Pretty Table Printer

-- a type for fill functions
data Align = AlignLeft | AlignRight | AlignCenter

-- functions that fill a string (s) to a given width (n) by adding pad
-- character (c) to align left, right, or center
fillLeft c n s = s <> L.replicate (n - L.length s) c
fillRight c n s = L.replicate (n - L.length s) c <> s
fillCenter c n s = L.replicate l c <> s <> L.replicate r c
    where x = n - L.length s
          l = x `div` 2
          r = x - l

-- converts a list of items into a table according to a list
-- of column descriptors
renderTable :: ([Align],[Align],[T.Text])
            -> [[T.Text]]
            -> Maybe [T.Text]
            -> T.Text
renderTable (headerAlign,rowAlign,header) rows footer =
    let widths = [maximum $ map L.length col |
                  col <- transpose $ (header : rows) ++ maybeToList footer]
                 :: [Int]
        separator = intercalateL "-+-" [L.replicate width '-' | width <- widths]
        fillCols fill cols = intercalateL " | " $
                             zipWith3 (($).) fill widths cols
    in
        L.unlines ( fillCols headerFill header
                  : separator
                  : map (fillCols rowFill) rows
                  ++ maybe []
                  (\x ->  [separator, fillCols rowFill x]) footer)
  where toFiller AlignLeft = fillLeft ' '
        toFiller AlignRight = fillRight ' '
        toFiller AlignCenter = fillCenter ' '
        headerFill = toFiller <$> headerAlign
        rowFill = toFiller <$> rowAlign
