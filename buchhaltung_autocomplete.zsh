autoload bashcompinit; bashcompinit
_buchhaltung()
{
    local cmdline
    local IFS=$'
'
    CMDLINE=(--bash-completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
    done

    COMPREPLY=( $(/home/johannes/.local/bin/buchhaltung "${CMDLINE[@]}") )
}

complete -o filenames -F _buchhaltung buchhaltung
