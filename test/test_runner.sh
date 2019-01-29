#!/usr/bin/env zsh
vim -Nu <( cat << EOF
  filetype off
  set backspace=start
  set rtp+=~/.vim/bundle/vader.vim
  set rtp+=~/.vim/bundle/auto_operator_spacing.vim
  filetype plugin indent on
  syntax enable
EOF
) +Vader! all.vader
