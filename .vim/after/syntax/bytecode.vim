" Vim syntax file
" Language: java bytecode
" Maintainer: Claudio Fleiner <claudio@fleiner.com>
" URL:    http://www.fleiner.com/vim/syntax/bytecode.vim
" Last change:  2001 Apr 26

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword bcKeyword nop aconst_null iconst_m1 iconst_0 iconst_1 iconst_2 iconst_3 iconst_4 iconst_5
syn keyword bcKeyword lconst_0 lconst_1 fconst_0 fconst_1 fconst_2 dconst_0 dconst_1 bipush
syn keyword bcKeyword sipush ldc ldc_w ldc2_w iload lload fload dload
syn keyword bcKeyword aload iload_0 iload_1 iload_2 iload_3 lload_0 lload_1 lload_2
syn keyword bcKeyword lload_3 fload_0 fload_1 fload_2 fload_3 dload_0 dload_1 dload_2
syn keyword bcKeyword dload_3 aload_0 aload_1 aload_2 aload_3 iaload laload faload
syn keyword bcKeyword daload aaload baload caload saload istore lstore fstore
syn keyword bcKeyword dstore astore istore_0 istore_1 istore_2 istore_3 lstore_0 lstore_1
syn keyword bcKeyword lstore_2 lstore_3 fstore_0 fstore_1 fstore_2 fstore_3 dstore_0 dstore_1
syn keyword bcKeyword dstore_2 dstore_3 astore_0 astore_1 astore_2 astore_3 iastore lastore
syn keyword bcKeyword fastore dastore aastore bastore castore sastore pop pop2
syn keyword bcKeyword dup dup_x1 dup_x2 dup2 dup2_x1 dup2_x2 swap iadd
syn keyword bcKeyword ladd fadd dadd isub lsub fsub dsub imul
syn keyword bcKeyword lmul fmul dmul idiv ldiv fdiv ddiv irem
syn keyword bcKeyword lrem frem drem ineg lneg fneg dneg ishl
syn keyword bcKeyword lshl ishr lshr iushr lushr iand land ior
syn keyword bcKeyword lor ixor lxor iinc i2l i2f i2d l2i
syn keyword bcKeyword l2f l2d f2i f2l f2d d2i d2l d2f
syn keyword bcKeyword i2b i2c i2s lcmp fcmpl fcmpg dcmpl dcmpg
syn keyword bcKeyword ifeq ifne iflt ifge ifgt ifle if_icmpeq if_icmpne
syn keyword bcKeyword if_icmplt if_icmpge if_icmpgt if_icmple if_acmpeq if_acmpne goto jsr
syn keyword bcKeyword ret tableswitch lookupswitch ireturn lreturn freturn dreturn areturn
syn keyword bcKeyword return getstatic putstatic getfield putfield invokevirtual invokespecial invokestatic
syn keyword bcKeyword invokeinterface new newarray anewarray arraylength athrow checkcast instanceof
syn keyword bcKeyword monitorenter monitorexit wide multianewarray ifnull ifnonnull goto_w jsr_w
syn keyword bcKeyword breakpoint ldc_quick ldc_w_quick ldc2_w_quick getfield_quick putfield_quick getfield2_quick putfield2_quick
syn keyword bcKeyword getstatic_quick putstatic_quick getstatic2_quick putstatic2_quick invokevirtual_quck invokenonvirtual_quick invokesuper_quick invokestatic_quick
syn keyword bcKeyword invokeinterface_quick aastore_quick new_quick anewarray_quick multianewarray_quick checkcast_quick instanceof_quick invokevirtual_quick_w
syn keyword bcKeyword getfield_quick_w putfield_quick_w nonnull_quick agetfield_quick aputfield_quick agetstatic_quick aputstatic_quick aldc_quick
syn keyword bcKeyword aldc_w_quick exit_sync_method sethi load_word_index load_short_index load_char_index load_byte_index load_ubyte_index
syn keyword bcKeyword store_word_index nastore_word_index store_short_index store_byte_index

syn keyword bcPicoKeyword load_ubyte load_byte load_char load_short load_word priv_ret_from_trap priv_read_dcache_tag priv_read_dcache_data
syn keyword bcPicoKeyword load_char_oe load_short_oe load_word_oe return0 priv_read_icache_tag priv_read_icache_data ncload_ubyte ncload_byte
syn keyword bcPicoKeyword ncload_char ncload_short ncload_word iucmp priv_powerdown cache_invalidate ncload_char_oe ncload_short_oe
syn keyword bcPicoKeyword ncload_word_oe return1 cache_flush cache_index_flush store_byte store_short store_word soft_trap
syn keyword bcPicoKeyword priv_write_dcache_tag priv_write_dcache_data store_short_oe store_word_oe return2 priv_write_icache_tag priv_write_icache_data ncstore_byte
syn keyword bcPicoKeyword ncstore_short ncstore_word priv_reset get_current_class ncstore_short_oe ncstore_word_oe call zero_line
syn keyword bcPicoKeyword priv_update_optop read_pc read_vars read_frame read_optop priv_read_oplim read_const_pool priv_read_psr
syn keyword bcPicoKeyword priv_read_trapbase priv_read_lockcount0 priv_read_lockcount1 priv_read_lockaddr0 priv_read_lockaddr1 priv_read_userrange1 priv_read_gc_config priv_read_brk1a
syn keyword bcPicoKeyword priv_read_brk2a priv_read_brk12c priv_read_versionid priv_read_hcr priv_read_sc_bottom read_global0 read_global1 read_global2
syn keyword bcPicoKeyword read_global3 write_pc write_vars write_frame write_optop priv_write_oplim write_const_pool priv_write_psr
syn keyword bcPicoKeyword priv_write_trapbase priv_write_lockcount0 priv_write_lockcount1 priv_write_lockaddr0 priv_write_lockaddr1 priv_write_userrange1 priv_write_gc_config priv_write_brk1a
syn keyword bcPicoKeyword priv_write_brk2a priv_write_brk12c priv_red_userrange2 priv_write_sc_bottom write_global0 write_global1 write_global2 write_global3

" assembler directives
syn match bcDirectives "\s\.\(type\|section\|sectioninfo\|sectionflag\|previous\|\d\=byte\|string\|ascii\|align\|size\|extern\|text\|set\|ident\|equ\|end\|block\)\>"ms=s+1
syn match bcGlobal "\s\.global\>"ms=s+1

" Comments
syn match bcComment "!.*$"
syn match bcComment "//.*$"
syn region bcComment start="/\*" end="\*/"

"integer number, or floating point number without a dot and with "f".
syn case ignore
syn match bcNumber    "\<\d\+\(u\=l\=\|lu\|f\)\>"
"floating point number, with dot, optional exponent
syn match bcFloat   "\<\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, starting with a dot, optional exponent
syn match bcFloat   "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match bcFloat   "\<\d\+e[-+]\=\d\+[fl]\=\>"
"hex number
syn match bcNumber    "\<0x\x\+\(u\=l\=\|lu\)\>"
" flag an octal number with wrong digits
syn match bcOctalError  "\<0\o*[89]"
syn case match

" Line (output from gdb only)
syn match bcLine "0x\x\+ <[^:]\+:"

"Address name (output from gdb only)
syn match bcAddress "<[^>]\+>"

" Labels
syn match bcLabel "^[A-Za-z.]\S*:"me=e-1
" syn match bcLabel "@@:\=\i\+"
" syn match bcCommands "^%\i\+"
" syn match bcCommands "%%"

syn match  bcSpecialError     contained "\\."
syn match  bcSpecialCharError contained "[^']"
syn match  bcSpecialChar      contained "\\\([4-9]\d\|[0-3]\d\d\|[\"\\'ntbrf]\|u\x\{4\}\)"
syn region bcString           start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=bcSpecialChar,bcSpecialError

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_bc_syn_inits")
  if version < 508
    let did_bc_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  let did_bc_syntax_inits = 1
  HiLink bcKeyword     Statement
  HiLink bcPicoKeyword Exception
  HiLink bcNumber     Number
  HiLink bcFloat      Number
  HiLink bcOctalError Error
  HiLink bcDirectives PreProc

  HiLink bcLine       Function
  HiLink bcComment    Comment
  HiLink bcAddress    Type
  HiLink bcConstant   Boolean
  HiLink bcAnnot      StorageClass
  HiLink bcCommands   Debug

  HiLink bcGlobal      Function
  HiLink bcLabel      Label
  HiLink bcString     String
  HiLink bcPreCondit  PreCondit
  HiLink bcInclude  Include
  HiLink bcIncluded String
  delcommand HiLink
endif
