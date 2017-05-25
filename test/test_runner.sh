vim -Nu <( cat << EOF
  filetype off
  set rtp+=~/.vim/bundle/vader.vim
  set rtp+=~/.vim/bundle/auto_operator_spacing.vim
  filetype plugin indent on
  syntax enable
EOF
) +Vader! $@
