let s:rule_categories = {}

func s:ensure_rule_list(category)
  if !has_key(s:rule_categories, a:category)
    let s:rule_categories[a:category] = []
  endif
  return s:rule_categories[a:category]
endfunc

func! s:check_for_duplicate_rule(category, text)
  if !has_key(s:rule_categories, a:category)
    return 0
  endif

  let l:rules = s:rule_categories[a:category]
  for r in l:rules
    if r[0] == a:text
      return 1
    endif
  endfor
  return 0
endfunc

func s:add_rule(category, text, action)
  if match(a:text, '\s') != -1
    echoerr 'Rules may not contain white space'
    return
  endif
  if s:check_for_duplicate_rule(a:category, a:text)
    echoerr 'Duplicate rule: "' . a:text . '"'
    return
  endif

  let l:list = split(a:text, '\zs')
  call map(l:list, {i, c -> escape(c, '/\')})
  let l:regex = '\V' . join(l:list, '\s\*') . '\%#'

  let l:rules = s:ensure_rule_list(a:category)
  call add(l:rules, [a:text, l:regex, a:action])
  " Note that the comparison here is backwards so we can achieve a greatest to
  " smallest ordering
  call sort(l:rules, {r, l -> strlen(r[0]) > strlen(l[0]) ? -1
                          \ : strlen(r[0]) == strlen(l[0]) ? 0 : 1})
endfunc

func s:remove_rule(category, text)
  let l:rules = s:ensure_rule_list(a:category)
  call filter(l:rules, {i, rule -> rule[0] != a:text})
endfunc

func s:attempt_respacing()
  if !exists("b:auto_operator_spacing_categories")
    return ''
  endif

  " After you call synstack the first time, its result changes for some
  " reason. The correct, from our perspective, result is the second one,
  " so we make a call here to get past the first value
  call synstack(line('.'), col('.') - 1)

  if exists('b:auto_operator_spacing_ignore_comments_and_strings') &&
   \ b:auto_operator_spacing_ignore_comments_and_strings &&
   \ eval(g:auto_operator_spacing_ignore_expr) != 0
    return ''
  endif

  let l:Categories = b:auto_operator_spacing_categories
  if type(l:Categories) == v:t_func
    let l:categories = l:Categories()
  endif
  if type(l:Categories) == v:t_string
    let l:categories = [l:Categories]
  elseif type(l:Categories) == v:t_list
    let l:categories = l:Categories
  endif
  unlet l:Categories

  let l:rule_lists = []
  for cat in l:categories
    if has_key(s:rule_categories, l:cat)
      call add(l:rule_lists, [s:rule_categories[l:cat], 0])
    endif
  endfor

  " We're going to be moving the cursor. Save the window state, so we can
  " restore it later.
  let l:start_col = col('.')
  let l:save_view = winsaveview()
  " Capital letter for MatchingRuleAction because it can be a Funcref
  let l:MatchingRuleAction = s:find_first_match(l:rule_lists)
  unlet l:rule_lists

  if type(l:MatchingRuleAction) == v:t_none
    " We didn't find a match.
    call winrestview(l:save_view)
    return ''
  endif

  " The cursor should now be positioned at the beginning of the matched
  " operator
  " Move the cursor to the beginning of any leading whitespace
  let l:pre_whitespace_col = col('.')
  call searchpos('\s*\%#', 'b')
  let l:post_whitespace_col = col('.')

  if type(l:MatchingRuleAction) == v:t_func
    " If MatchingRuleAction is a Funcref, evaluate it
    let l:action = l:MatchingRuleAction()
  else
    let l:action = l:MatchingRuleAction
  endif
  unlet l:MatchingRuleAction

  if type(l:action) == v:t_none
    " This is a none-action
    call winrestview(l:save_view)
    return ''
  endif

  if l:post_whitespace_col == 1
    " If this is at the start of a line (ignorning whitespace) then don't
    " insert any whitespace before the operator and don't remove any
    " preceding whitespace
    let l:action = substitute(l:action, '^\s\+', '', 'g')
    let l:backspaces = l:start_col - l:pre_whitespace_col
  else
    let l:backspaces = l:start_col - l:post_whitespace_col
  endif

  " Restore the cursor to its original position
  call winrestview(l:save_view)
  return repeat("\<BS>", l:backspaces) . l:action
endfunc

" A helper to find the longest matching rule. Each rule list is sorted, so if
" we treverse all of them in parallel, the first match we find is the longest.
func s:find_first_match(rule_lists)
  while 1
    let l:max_len = 0
    for [l:rules, l:i] in a:rule_lists
      if l:i < len(l:rules) && len(l:rules[l:i][0]) > l:max_len
        let l:max_len = len(l:rules[l:i][0])
      endif
    endfor
    if l:max_len <= 0
      return v:none
    endif

    for rl in a:rule_lists
      while 1
        let [rules, i] =  rl
        if l:i >= len(l:rules) || len(l:rules[l:i][0]) < l:max_len
          break
        endif

        if search(l:rules[l:i][1], 'bW') > 0
          return l:rules[l:i][2]
        endif

        let l:rl[1] += 1
      endwhile
    endfor
  endwhile
endfunc

func s:add_mapping(char)
  if strchars(a:char) != 1
    echoerr 'Mappings must exactly one character in length'
  endif
  let l:char = escape(a:char, '|')
  "exec "imap <buffer> " . l:char . " " . l:char . "<Plug>AutoOperatorSpacing"
  exec "inoremap <script><buffer> " . l:char . " "
                \ . l:char . "<C-R>=<SID>attempt_respacing()<CR>"
endfunc


func s:add_standard_mappings()
  AutoOperatorSpacingAddMapping =
  AutoOperatorSpacingAddMapping +
  AutoOperatorSpacingAddMapping !
  AutoOperatorSpacingAddMapping #
  AutoOperatorSpacingAddMapping %
  AutoOperatorSpacingAddMapping ^
  AutoOperatorSpacingAddMapping &
  AutoOperatorSpacingAddMapping *
  AutoOperatorSpacingAddMapping -
  AutoOperatorSpacingAddMapping |
  AutoOperatorSpacingAddMapping ;
  AutoOperatorSpacingAddMapping :
  AutoOperatorSpacingAddMapping ,
  AutoOperatorSpacingAddMapping <
  AutoOperatorSpacingAddMapping .
  AutoOperatorSpacingAddMapping >
  AutoOperatorSpacingAddMapping /
  AutoOperatorSpacingAddMapping ?
endfunc

"inoremap <Plug>AutoOperatorSpacing <C-R>=<SID>attempt_respacing()<CR>

command -nargs=1 AutoOperatorSpacingAddMapping :call s:add_mapping('<args>')
command -nargs=0 AutoOperatorSpacingStandardMappings :call s:add_standard_mappings()
command -nargs=+ AutoOperatorSpacingAddRule :call s:add_rule(<args>)

" Helpers/Utilities {{{
" Stolen from matchparen.vim
let g:auto_operator_spacing_ignore_expr =
    \ '!empty(filter(map(synstack(line("."), col(".") - 1), ''synIDattr(v:val, "name")''), ' .
    \ '''v:val =~? "string\\|character\\|singlequote\\|escape\\|comment"''))'
let g:auto_operator_spacing_ignore_expr_searchpair =
    \ '!empty(filter(map(synstack(line("."), col(".")), ''synIDattr(v:val, "name")''), ' .
    \ '''v:val =~? "string\\|character\\|singlequote\\|escape\\|comment"''))'
func s:probably_unary()
  return search('\V\[=+^&*\-|;:,<>/]\_s\*\%#', 'bnW') != 0 ||
       \ search('\V\(\<return\>\|\<yield\>\)\_s\*\%#', 'bnW') != 0
endfunc

func s:compare_pos(a, b)
  if a:a[0] < a:b[0]
    return -1
  elseif a:a[0] > a:b[0]
    return 1
  elseif a:a[1] < a:b[1]
    return -1
  elseif a:a[1] > a:b[1]
    return 1
  else
    return 0
  endif
endfunc

" Returns the type of the nearest open delimeter (one of (, [, or {)
func s:nearest_open_delimeter()
  let paren_pos = searchpairpos('(', '', ')', 'nb', g:auto_operator_spacing_ignore_expr_searchpair)
  let bracket_pos = searchpairpos('\[', '', '\]', 'nb', g:auto_operator_spacing_ignore_expr_searchpair)
  let brace_pos = searchpairpos('{', '', '}', 'nb', g:auto_operator_spacing_ignore_expr_searchpair)
  if paren_pos == [0, 0] && bracket_pos == [0, 0] && brace_pos == [0, 0]
    return ''
  endif
  if s:compare_pos(paren_pos, bracket_pos) > 0
    if s:compare_pos(paren_pos, brace_pos) > 0
      return '('
    elseif s:compare_pos(bracket_pos, brace_pos) > 0
      return '['
    else
      return '{'
    endif
  elseif s:compare_pos(bracket_pos, brace_pos) > 0
    return '['
  else
    return '{'
  endif
endfunc
" }}

" Generic rules {{{
func s:generic_binary_operator(op)
  AutoOperatorSpacingAddRule "generic", a:op, ' ' . a:op .' '
  AutoOperatorSpacingAddRule "generic", a:op . "=", ' ' . a:op .'= '
endfunc

call s:generic_binary_operator('+')
call s:generic_binary_operator('*')
call s:generic_binary_operator('/')
call s:generic_binary_operator('%')
call s:generic_binary_operator('&')
call s:generic_binary_operator('|')
call s:generic_binary_operator('=')
call s:generic_binary_operator('>')
call s:generic_binary_operator('>>')
call s:generic_binary_operator('<<')

AutoOperatorSpacingAddRule "generic", "!=", ' != '
AutoOperatorSpacingAddRule "generic", ",", ', '
AutoOperatorSpacingAddRule "generic", ":", ': '
AutoOperatorSpacingAddRule "generic", "&&", ' && '
AutoOperatorSpacingAddRule "generic", "||", ' || '

func s:generic_minus()
  if s:probably_unary()
    return ' -'
  elseif search('[[({]\_s*\%#', 'bnW') != 0
    return '-'
  else
    return ' - '
  endif
endfunc
AutoOperatorSpacingAddRule "generic", "-", funcref('s:generic_minus')
" }}}

" Python rules {{{
AutoOperatorSpacingAddRule "python", "#", ' # '

func s:python_star(op)
  if search('\V,\_s\*\%#', 'bnW') != 0
    return ' ' . a:op
  elseif search('\V(\_s\*\%#', 'bnW') != 0
    return a:op
  else
    return ' ' . a:op . ' '
  endif
endfunc

AutoOperatorSpacingAddRule "python", "*", {-> s:python_star('*')}
AutoOperatorSpacingAddRule "python", "**", {-> s:python_star('**')}
AutoOperatorSpacingAddRule "python", "//", ' // '



func s:python_colon()
  if s:nearest_open_delimeter() == '['
    " Array slicing case
    return ':'
  else
    " For all other cases we'll insert a trailing space and accept that this
    " could result in a space at the end of the line.
    return ': '
  endif
endfunc
AutoOperatorSpacingAddRule "python", ":", funcref('s:python_colon')

func s:python_equals()
  " Defaults in function definations and named args in function calls
  if s:nearest_open_delimeter() == '('
    return '='
  else
    return ' = '
  endif
endfunc
AutoOperatorSpacingAddRule "python", "=", funcref('s:python_equals')

func s:python_minus()
  " Special case for l[:-1]
  if s:nearest_open_delimeter() == '[' && search('\_s*\%#', 'bnW')
    return '-'
  else
    return s:generic_minus()
  endif
endfunc
AutoOperatorSpacingAddRule "python", "-", funcref('s:python_minus')
" }}}

" Rust rules {{{
AutoOperatorSpacingAddRule "rust", ";", '; '
AutoOperatorSpacingAddRule "rust", "->", ' -> '
AutoOperatorSpacingAddRule "rust", "=>", ' => '
AutoOperatorSpacingAddRule "rust", "//", '// '
AutoOperatorSpacingAddRule "rust", "/*", '/* '

let s:rust_keywords = join(
  \ ['\<return\>', '\<yield\>', '\<match\>', '\<as\>', '\<where\>', '\<mut\>',
  \  '\<if\>', '\<while\>', '\<in\>', '\<use\>', '\<impl\>'],
  \ '\|'
\)
" A slightly tweaked version of s:probably_unary. Handles rust keywords and
" lifetimes
func s:rust_probably_unary()
  return search('\(' . s:rust_keywords . '\|''[a-z]\+\|[=+^&*\-|;:,>/]\)\_s*\%#', 'bnW') != 0
endfunc

func s:rust_scope()
  if s:rust_probably_unary() != 0
    return ' ::'
  else
    return '::'
  endif
endfunc
AutoOperatorSpacingAddRule "rust", '::', funcref('s:rust_scope')

" A couple of heuristics to guess if we're in type position or an unary
" operator.
func s:rust_ampersand_or_star(op)

  " To handle chains of of & and *, move the cursor to the start of the first
  " one we can find, and see if it is a unary operator or not
  if search('\v([*&]|\_s)+%#', 'bW') != 0
    if s:rust_probably_unary() || search('[[({]\_s*\%#', 'bnW') != 0
      return a:op
    else
      return ' ' . a:op
    endif
  endif

  " If the operator is preceeded by '<', and that looks like a typename, treat
  " this like a typename
  if search('<\+\_s*\%#', 'bW') != 0
    let l:n = s:rust_open_angle()
    if l:n == '<' || l:n == ' <'
      return a:op
    else
      return ' ' . a:po
    endif
  elseif search('[[({]\_s*\%#', 'bnW') != 0
    return a:op
  elseif s:rust_probably_unary()
    return ' ' . a:op
  else
    return ' ' . a:op . ' '
  endif
endfunc
AutoOperatorSpacingAddRule "rust", "*", {->s:rust_ampersand_or_star('*')}
AutoOperatorSpacingAddRule "rust", "&", {->s:rust_ampersand_or_star('&')}
AutoOperatorSpacingAddRule "rust", "&&", {->s:rust_ampersand_or_star('&&')}

func s:rust_open_angle()
  if search('\([[({]\|::\|\<impl\)\_s*\%#', 'bnW') != 0
   " Probably a trait-disambiguation <Vec<u8> as IntoIterator>
    return '<'
  elseif s:rust_probably_unary()
    " ^^^ Ditto ^^^
    return ' <'
  elseif search('\(fn\s\+[a-z0-9_]*\|[A-Z][a-z]*\)\_s*\%#', 'bnW') != 0
    " Since this follows a CamelCase identifier, its probably a type name
    return '<'
  else
    " Binary expression as a fall back
    return ' < '
  endif
endfunc
AutoOperatorSpacingAddRule "rust", "<", funcref('s:rust_open_angle')

func s:rust_close_angle_shared(binary_op, typename_op)
  " We base our decision off of the opening angle bracket, so try to find it
  let l:found = searchpair('<', '', '>', 'b', g:auto_operator_spacing_ignore_expr)
  if l:found <= 0
    return a:binary_op
  endif

  let l:opening = s:rust_open_angle()

  if l:opening == '<' || l:opening == ' <'
    return a:typename_op
  else
    return a:binary_op
  endif
endfunc
AutoOperatorSpacingAddRule "rust", ">", {-> s:rust_close_angle_shared(' > ', '>')}
AutoOperatorSpacingAddRule "rust", ">=", {-> s:rust_close_angle_shared(' >= ', '> = ')}
AutoOperatorSpacingAddRule "rust", ">>", {-> s:rust_close_angle_shared(' >> ', '>>')}
AutoOperatorSpacingAddRule "rust", ">>=", {-> s:rust_close_angle_shared(' >>= ', '>> = ')}

func s:rust_pipe()
  if s:rust_probably_unary()
    return ' |'
  elseif search('[[({]\_s*\%#', 'bnW') != 0
    return '|'
  endif

  " To test if this is the end of a lambda's parameter list, look for an
  " opening pipe and see if it looks like an unary operation.
  if search('[^|]\+\%#', 'bW', line('.')) > 0
    if s:rust_probably_unary()
      return '| '
    endif
  endif
  return ' | '
endfunc
AutoOperatorSpacingAddRule "rust", "|", funcref('s:rust_pipe')
" }}}