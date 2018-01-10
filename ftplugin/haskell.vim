AutoOperatorSpacingStandardMappings

let b:auto_operator_spacing_categories = ["haskell"]
let b:auto_operator_spacing_ignore_comments_and_strings = 1

let s:undo_ftplugin = "unlet b:auto_operator_spacing_categories"
if exists("b:undo_ftplugin")
  let b:undo_ftplugin = b:undo_ftplugin . " | " . s:undo_ftplugin
else
  let b:undo_ftplugin = s:undo_ftplugin
endif
