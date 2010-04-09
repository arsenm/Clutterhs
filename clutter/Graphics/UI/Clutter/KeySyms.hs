module Graphics.UI.Clutter.KeySyms where

import Data.Word

type KeySym = Word32

kVoidSymbol :: KeySym
kVoidSymbol = 0xffffff
 
kBackSpace :: KeySym
kBackSpace = 0xff08
 
kTab :: KeySym
kTab = 0xff09
 
kLinefeed :: KeySym
kLinefeed = 0xff0a
 
kClear :: KeySym
kClear = 0xff0b
 
kReturn :: KeySym
kReturn = 0xff0d
 
kPause :: KeySym
kPause = 0xff13
 
kScroll_Lock :: KeySym
kScroll_Lock = 0xff14
 
kSys_Req :: KeySym
kSys_Req = 0xff15
 
kEscape :: KeySym
kEscape = 0xff1b
 
kDelete :: KeySym
kDelete = 0xffff
 
kMulti_key :: KeySym
kMulti_key = 0xff20
 
kCodeinput :: KeySym
kCodeinput = 0xff37
 
kSingleCandidate :: KeySym
kSingleCandidate = 0xff3c
 
kMultipleCandidate :: KeySym
kMultipleCandidate = 0xff3d
 
kPreviousCandidate :: KeySym
kPreviousCandidate = 0xff3e
 
kKanji :: KeySym
kKanji = 0xff21
 
kMuhenkan :: KeySym
kMuhenkan = 0xff22
 
kHenkan_Mode :: KeySym
kHenkan_Mode = 0xff23
 
kHenkan :: KeySym
kHenkan = 0xff23
 
kRomaji :: KeySym
kRomaji = 0xff24
 
kHiragana :: KeySym
kHiragana = 0xff25
 
kKatakana :: KeySym
kKatakana = 0xff26
 
kHiragana_Katakana :: KeySym
kHiragana_Katakana = 0xff27
 
kZenkaku :: KeySym
kZenkaku = 0xff28
 
kHankaku :: KeySym
kHankaku = 0xff29
 
kZenkaku_Hankaku :: KeySym
kZenkaku_Hankaku = 0xff2a
 
kTouroku :: KeySym
kTouroku = 0xff2b
 
kMassyo :: KeySym
kMassyo = 0xff2c
 
kKana_Lock :: KeySym
kKana_Lock = 0xff2d
 
kKana_Shift :: KeySym
kKana_Shift = 0xff2e
 
kEisu_Shift :: KeySym
kEisu_Shift = 0xff2f
 
kEisu_toggle :: KeySym
kEisu_toggle = 0xff30
 
kKanji_Bangou :: KeySym
kKanji_Bangou = 0xff37
 
kZen_Koho :: KeySym
kZen_Koho = 0xff3d
 
kMae_Koho :: KeySym
kMae_Koho = 0xff3e
 
kHome :: KeySym
kHome = 0xff50
 
kLeft :: KeySym
kLeft = 0xff51
 
kUp :: KeySym
kUp = 0xff52
 
kRight :: KeySym
kRight = 0xff53
 
kDown :: KeySym
kDown = 0xff54
 
kPrior :: KeySym
kPrior = 0xff55
 
kPage_Up :: KeySym
kPage_Up = 0xff55
 
kNext :: KeySym
kNext = 0xff56
 
kPage_Down :: KeySym
kPage_Down = 0xff56
 
kEnd :: KeySym
kEnd = 0xff57
 
kBegin :: KeySym
kBegin = 0xff58
 
kSelect :: KeySym
kSelect = 0xff60
 
kPrint :: KeySym
kPrint = 0xff61
 
kExecute :: KeySym
kExecute = 0xff62
 
kInsert :: KeySym
kInsert = 0xff63
 
kUndo :: KeySym
kUndo = 0xff65
 
kRedo :: KeySym
kRedo = 0xff66
 
kMenu :: KeySym
kMenu = 0xff67
 
kFind :: KeySym
kFind = 0xff68
 
kCancel :: KeySym
kCancel = 0xff69
 
kHelp :: KeySym
kHelp = 0xff6a
 
kBreak :: KeySym
kBreak = 0xff6b
 
kMode_switch :: KeySym
kMode_switch = 0xff7e
 
kscript_switch :: KeySym
kscript_switch = 0xff7e
 
kNum_Lock :: KeySym
kNum_Lock = 0xff7f
 
kKP_Space :: KeySym
kKP_Space = 0xff80
 
kKP_Tab :: KeySym
kKP_Tab = 0xff89
 
kKP_Enter :: KeySym
kKP_Enter = 0xff8d
 
kKP_F1 :: KeySym
kKP_F1 = 0xff91
 
kKP_F2 :: KeySym
kKP_F2 = 0xff92
 
kKP_F3 :: KeySym
kKP_F3 = 0xff93
 
kKP_F4 :: KeySym
kKP_F4 = 0xff94
 
kKP_Home :: KeySym
kKP_Home = 0xff95
 
kKP_Left :: KeySym
kKP_Left = 0xff96
 
kKP_Up :: KeySym
kKP_Up = 0xff97
 
kKP_Right :: KeySym
kKP_Right = 0xff98
 
kKP_Down :: KeySym
kKP_Down = 0xff99
 
kKP_Prior :: KeySym
kKP_Prior = 0xff9a
 
kKP_Page_Up :: KeySym
kKP_Page_Up = 0xff9a
 
kKP_Next :: KeySym
kKP_Next = 0xff9b
 
kKP_Page_Down :: KeySym
kKP_Page_Down = 0xff9b
 
kKP_End :: KeySym
kKP_End = 0xff9c
 
kKP_Begin :: KeySym
kKP_Begin = 0xff9d
 
kKP_Insert :: KeySym
kKP_Insert = 0xff9e
 
kKP_Delete :: KeySym
kKP_Delete = 0xff9f
 
kKP_Equal :: KeySym
kKP_Equal = 0xffbd
 
kKP_Multiply :: KeySym
kKP_Multiply = 0xffaa
 
kKP_Add :: KeySym
kKP_Add = 0xffab
 
kKP_Separator :: KeySym
kKP_Separator = 0xffac
 
kKP_Subtract :: KeySym
kKP_Subtract = 0xffad
 
kKP_Decimal :: KeySym
kKP_Decimal = 0xffae
 
kKP_Divide :: KeySym
kKP_Divide = 0xffaf
 
kKP_0 :: KeySym
kKP_0 = 0xffb0
 
kKP_1 :: KeySym
kKP_1 = 0xffb1
 
kKP_2 :: KeySym
kKP_2 = 0xffb2
 
kKP_3 :: KeySym
kKP_3 = 0xffb3
 
kKP_4 :: KeySym
kKP_4 = 0xffb4
 
kKP_5 :: KeySym
kKP_5 = 0xffb5
 
kKP_6 :: KeySym
kKP_6 = 0xffb6
 
kKP_7 :: KeySym
kKP_7 = 0xffb7
 
kKP_8 :: KeySym
kKP_8 = 0xffb8
 
kKP_9 :: KeySym
kKP_9 = 0xffb9
 
kF1 :: KeySym
kF1 = 0xffbe
 
kF2 :: KeySym
kF2 = 0xffbf
 
kF3 :: KeySym
kF3 = 0xffc0
 
kF4 :: KeySym
kF4 = 0xffc1
 
kF5 :: KeySym
kF5 = 0xffc2
 
kF6 :: KeySym
kF6 = 0xffc3
 
kF7 :: KeySym
kF7 = 0xffc4
 
kF8 :: KeySym
kF8 = 0xffc5
 
kF9 :: KeySym
kF9 = 0xffc6
 
kF10 :: KeySym
kF10 = 0xffc7
 
kF11 :: KeySym
kF11 = 0xffc8
 
kL1 :: KeySym
kL1 = 0xffc8
 
kF12 :: KeySym
kF12 = 0xffc9
 
kL2 :: KeySym
kL2 = 0xffc9
 
kF13 :: KeySym
kF13 = 0xffca
 
kL3 :: KeySym
kL3 = 0xffca
 
kF14 :: KeySym
kF14 = 0xffcb
 
kL4 :: KeySym
kL4 = 0xffcb
 
kF15 :: KeySym
kF15 = 0xffcc
 
kL5 :: KeySym
kL5 = 0xffcc
 
kF16 :: KeySym
kF16 = 0xffcd
 
kL6 :: KeySym
kL6 = 0xffcd
 
kF17 :: KeySym
kF17 = 0xffce
 
kL7 :: KeySym
kL7 = 0xffce
 
kF18 :: KeySym
kF18 = 0xffcf
 
kL8 :: KeySym
kL8 = 0xffcf
 
kF19 :: KeySym
kF19 = 0xffd0
 
kL9 :: KeySym
kL9 = 0xffd0
 
kF20 :: KeySym
kF20 = 0xffd1
 
kL10 :: KeySym
kL10 = 0xffd1
 
kF21 :: KeySym
kF21 = 0xffd2
 
kR1 :: KeySym
kR1 = 0xffd2
 
kF22 :: KeySym
kF22 = 0xffd3
 
kR2 :: KeySym
kR2 = 0xffd3
 
kF23 :: KeySym
kF23 = 0xffd4
 
kR3 :: KeySym
kR3 = 0xffd4
 
kF24 :: KeySym
kF24 = 0xffd5
 
kR4 :: KeySym
kR4 = 0xffd5
 
kF25 :: KeySym
kF25 = 0xffd6
 
kR5 :: KeySym
kR5 = 0xffd6
 
kF26 :: KeySym
kF26 = 0xffd7
 
kR6 :: KeySym
kR6 = 0xffd7
 
kF27 :: KeySym
kF27 = 0xffd8
 
kR7 :: KeySym
kR7 = 0xffd8
 
kF28 :: KeySym
kF28 = 0xffd9
 
kR8 :: KeySym
kR8 = 0xffd9
 
kF29 :: KeySym
kF29 = 0xffda
 
kR9 :: KeySym
kR9 = 0xffda
 
kF30 :: KeySym
kF30 = 0xffdb
 
kR10 :: KeySym
kR10 = 0xffdb
 
kF31 :: KeySym
kF31 = 0xffdc
 
kR11 :: KeySym
kR11 = 0xffdc
 
kF32 :: KeySym
kF32 = 0xffdd
 
kR12 :: KeySym
kR12 = 0xffdd
 
kF33 :: KeySym
kF33 = 0xffde
 
kR13 :: KeySym
kR13 = 0xffde
 
kF34 :: KeySym
kF34 = 0xffdf
 
kR14 :: KeySym
kR14 = 0xffdf
 
kF35 :: KeySym
kF35 = 0xffe0
 
kR15 :: KeySym
kR15 = 0xffe0
 
kShift_L :: KeySym
kShift_L = 0xffe1
 
kShift_R :: KeySym
kShift_R = 0xffe2
 
kControl_L :: KeySym
kControl_L = 0xffe3
 
kControl_R :: KeySym
kControl_R = 0xffe4
 
kCaps_Lock :: KeySym
kCaps_Lock = 0xffe5
 
kShift_Lock :: KeySym
kShift_Lock = 0xffe6
 
kMeta_L :: KeySym
kMeta_L = 0xffe7
 
kMeta_R :: KeySym
kMeta_R = 0xffe8
 
kAlt_L :: KeySym
kAlt_L = 0xffe9
 
kAlt_R :: KeySym
kAlt_R = 0xffea
 
kSuper_L :: KeySym
kSuper_L = 0xffeb
 
kSuper_R :: KeySym
kSuper_R = 0xffec
 
kHyper_L :: KeySym
kHyper_L = 0xffed
 
kHyper_R :: KeySym
kHyper_R = 0xffee
 
kISO_Lock :: KeySym
kISO_Lock = 0xfe01
 
kISO_Level2_Latch :: KeySym
kISO_Level2_Latch = 0xfe02
 
kISO_Level3_Shift :: KeySym
kISO_Level3_Shift = 0xfe03
 
kISO_Level3_Latch :: KeySym
kISO_Level3_Latch = 0xfe04
 
kISO_Level3_Lock :: KeySym
kISO_Level3_Lock = 0xfe05
 
kISO_Level5_Shift :: KeySym
kISO_Level5_Shift = 0xfe11
 
kISO_Level5_Latch :: KeySym
kISO_Level5_Latch = 0xfe12
 
kISO_Level5_Lock :: KeySym
kISO_Level5_Lock = 0xfe13
 
kISO_Group_Shift :: KeySym
kISO_Group_Shift = 0xff7e
 
kISO_Group_Latch :: KeySym
kISO_Group_Latch = 0xfe06
 
kISO_Group_Lock :: KeySym
kISO_Group_Lock = 0xfe07
 
kISO_Next_Group :: KeySym
kISO_Next_Group = 0xfe08
 
kISO_Next_Group_Lock :: KeySym
kISO_Next_Group_Lock = 0xfe09
 
kISO_Prev_Group :: KeySym
kISO_Prev_Group = 0xfe0a
 
kISO_Prev_Group_Lock :: KeySym
kISO_Prev_Group_Lock = 0xfe0b
 
kISO_First_Group :: KeySym
kISO_First_Group = 0xfe0c
 
kISO_First_Group_Lock :: KeySym
kISO_First_Group_Lock = 0xfe0d
 
kISO_Last_Group :: KeySym
kISO_Last_Group = 0xfe0e
 
kISO_Last_Group_Lock :: KeySym
kISO_Last_Group_Lock = 0xfe0f
 
kISO_Left_Tab :: KeySym
kISO_Left_Tab = 0xfe20
 
kISO_Move_Line_Up :: KeySym
kISO_Move_Line_Up = 0xfe21
 
kISO_Move_Line_Down :: KeySym
kISO_Move_Line_Down = 0xfe22
 
kISO_Partial_Line_Up :: KeySym
kISO_Partial_Line_Up = 0xfe23
 
kISO_Partial_Line_Down :: KeySym
kISO_Partial_Line_Down = 0xfe24
 
kISO_Partial_Space_Left :: KeySym
kISO_Partial_Space_Left = 0xfe25
 
kISO_Partial_Space_Right :: KeySym
kISO_Partial_Space_Right = 0xfe26
 
kISO_Set_Margin_Left :: KeySym
kISO_Set_Margin_Left = 0xfe27
 
kISO_Set_Margin_Right :: KeySym
kISO_Set_Margin_Right = 0xfe28
 
kISO_Release_Margin_Left :: KeySym
kISO_Release_Margin_Left = 0xfe29
 
kISO_Release_Margin_Right :: KeySym
kISO_Release_Margin_Right = 0xfe2a
 
kISO_Release_Both_Margins :: KeySym
kISO_Release_Both_Margins = 0xfe2b
 
kISO_Fast_Cursor_Left :: KeySym
kISO_Fast_Cursor_Left = 0xfe2c
 
kISO_Fast_Cursor_Right :: KeySym
kISO_Fast_Cursor_Right = 0xfe2d
 
kISO_Fast_Cursor_Up :: KeySym
kISO_Fast_Cursor_Up = 0xfe2e
 
kISO_Fast_Cursor_Down :: KeySym
kISO_Fast_Cursor_Down = 0xfe2f
 
kISO_Continuous_Underline :: KeySym
kISO_Continuous_Underline = 0xfe30
 
kISO_Discontinuous_Underline :: KeySym
kISO_Discontinuous_Underline = 0xfe31
 
kISO_Emphasize :: KeySym
kISO_Emphasize = 0xfe32
 
kISO_Center_Object :: KeySym
kISO_Center_Object = 0xfe33
 
kISO_Enter :: KeySym
kISO_Enter = 0xfe34
 
kdead_grave :: KeySym
kdead_grave = 0xfe50
 
kdead_acute :: KeySym
kdead_acute = 0xfe51
 
kdead_circumflex :: KeySym
kdead_circumflex = 0xfe52
 
kdead_tilde :: KeySym
kdead_tilde = 0xfe53
 
kdead_perispomeni :: KeySym
kdead_perispomeni = 0xfe53
 
kdead_macron :: KeySym
kdead_macron = 0xfe54
 
kdead_breve :: KeySym
kdead_breve = 0xfe55
 
kdead_abovedot :: KeySym
kdead_abovedot = 0xfe56
 
kdead_diaeresis :: KeySym
kdead_diaeresis = 0xfe57
 
kdead_abovering :: KeySym
kdead_abovering = 0xfe58
 
kdead_doubleacute :: KeySym
kdead_doubleacute = 0xfe59
 
kdead_caron :: KeySym
kdead_caron = 0xfe5a
 
kdead_cedilla :: KeySym
kdead_cedilla = 0xfe5b
 
kdead_ogonek :: KeySym
kdead_ogonek = 0xfe5c
 
kdead_iota :: KeySym
kdead_iota = 0xfe5d
 
kdead_voiced_sound :: KeySym
kdead_voiced_sound = 0xfe5e
 
kdead_semivoiced_sound :: KeySym
kdead_semivoiced_sound = 0xfe5f
 
kdead_belowdot :: KeySym
kdead_belowdot = 0xfe60
 
kdead_hook :: KeySym
kdead_hook = 0xfe61
 
kdead_horn :: KeySym
kdead_horn = 0xfe62
 
kdead_stroke :: KeySym
kdead_stroke = 0xfe63
 
kdead_abovecomma :: KeySym
kdead_abovecomma = 0xfe64
 
kdead_psili :: KeySym
kdead_psili = 0xfe64
 
kdead_abovereversedcomma :: KeySym
kdead_abovereversedcomma = 0xfe65
 
kdead_dasia :: KeySym
kdead_dasia = 0xfe65
 
kdead_belowring :: KeySym
kdead_belowring = 0xfe67
 
kdead_belowmacron :: KeySym
kdead_belowmacron = 0xfe68
 
kdead_belowcircumflex :: KeySym
kdead_belowcircumflex = 0xfe69
 
kdead_belowtilde :: KeySym
kdead_belowtilde = 0xfe6a
 
kdead_belowbreve :: KeySym
kdead_belowbreve = 0xfe6b
 
kdead_belowdiaeresis :: KeySym
kdead_belowdiaeresis = 0xfe6c
 
kFirst_Virtual_Screen :: KeySym
kFirst_Virtual_Screen = 0xfed0
 
kPrev_Virtual_Screen :: KeySym
kPrev_Virtual_Screen = 0xfed1
 
kNext_Virtual_Screen :: KeySym
kNext_Virtual_Screen = 0xfed2
 
kLast_Virtual_Screen :: KeySym
kLast_Virtual_Screen = 0xfed4
 
kTerminate_Server :: KeySym
kTerminate_Server = 0xfed5
 
kAccessX_Enable :: KeySym
kAccessX_Enable = 0xfe70
 
kAccessX_Feedback_Enable :: KeySym
kAccessX_Feedback_Enable = 0xfe71
 
kRepeatKeys_Enable :: KeySym
kRepeatKeys_Enable = 0xfe72
 
kSlowKeys_Enable :: KeySym
kSlowKeys_Enable = 0xfe73
 
kBounceKeys_Enable :: KeySym
kBounceKeys_Enable = 0xfe74
 
kStickyKeys_Enable :: KeySym
kStickyKeys_Enable = 0xfe75
 
kMouseKeys_Enable :: KeySym
kMouseKeys_Enable = 0xfe76
 
kMouseKeys_Accel_Enable :: KeySym
kMouseKeys_Accel_Enable = 0xfe77
 
kOverlay1_Enable :: KeySym
kOverlay1_Enable = 0xfe78
 
kOverlay2_Enable :: KeySym
kOverlay2_Enable = 0xfe79
 
kAudibleBell_Enable :: KeySym
kAudibleBell_Enable = 0xfe7a
 
kPointer_Left :: KeySym
kPointer_Left = 0xfee0
 
kPointer_Right :: KeySym
kPointer_Right = 0xfee1
 
kPointer_Up :: KeySym
kPointer_Up = 0xfee2
 
kPointer_Down :: KeySym
kPointer_Down = 0xfee3
 
kPointer_UpLeft :: KeySym
kPointer_UpLeft = 0xfee4
 
kPointer_UpRight :: KeySym
kPointer_UpRight = 0xfee5
 
kPointer_DownLeft :: KeySym
kPointer_DownLeft = 0xfee6
 
kPointer_DownRight :: KeySym
kPointer_DownRight = 0xfee7
 
kPointer_Button_Dflt :: KeySym
kPointer_Button_Dflt = 0xfee8
 
kPointer_Button1 :: KeySym
kPointer_Button1 = 0xfee9
 
kPointer_Button2 :: KeySym
kPointer_Button2 = 0xfeea
 
kPointer_Button3 :: KeySym
kPointer_Button3 = 0xfeeb
 
kPointer_Button4 :: KeySym
kPointer_Button4 = 0xfeec
 
kPointer_Button5 :: KeySym
kPointer_Button5 = 0xfeed
 
kPointer_DblClick_Dflt :: KeySym
kPointer_DblClick_Dflt = 0xfeee
 
kPointer_DblClick1 :: KeySym
kPointer_DblClick1 = 0xfeef
 
kPointer_DblClick2 :: KeySym
kPointer_DblClick2 = 0xfef0
 
kPointer_DblClick3 :: KeySym
kPointer_DblClick3 = 0xfef1
 
kPointer_DblClick4 :: KeySym
kPointer_DblClick4 = 0xfef2
 
kPointer_DblClick5 :: KeySym
kPointer_DblClick5 = 0xfef3
 
kPointer_Drag_Dflt :: KeySym
kPointer_Drag_Dflt = 0xfef4
 
kPointer_Drag1 :: KeySym
kPointer_Drag1 = 0xfef5
 
kPointer_Drag2 :: KeySym
kPointer_Drag2 = 0xfef6
 
kPointer_Drag3 :: KeySym
kPointer_Drag3 = 0xfef7
 
kPointer_Drag4 :: KeySym
kPointer_Drag4 = 0xfef8
 
kPointer_Drag5 :: KeySym
kPointer_Drag5 = 0xfefd
 
kPointer_EnableKeys :: KeySym
kPointer_EnableKeys = 0xfef9
 
kPointer_Accelerate :: KeySym
kPointer_Accelerate = 0xfefa
 
kPointer_DfltBtnNext :: KeySym
kPointer_DfltBtnNext = 0xfefb
 
kPointer_DfltBtnPrev :: KeySym
kPointer_DfltBtnPrev = 0xfefc
 
k3270_Duplicate :: KeySym
k3270_Duplicate = 0xfd01
 
k3270_FieldMark :: KeySym
k3270_FieldMark = 0xfd02
 
k3270_Right2 :: KeySym
k3270_Right2 = 0xfd03
 
k3270_Left2 :: KeySym
k3270_Left2 = 0xfd04
 
k3270_BackTab :: KeySym
k3270_BackTab = 0xfd05
 
k3270_EraseEOF :: KeySym
k3270_EraseEOF = 0xfd06
 
k3270_EraseInput :: KeySym
k3270_EraseInput = 0xfd07
 
k3270_Reset :: KeySym
k3270_Reset = 0xfd08
 
k3270_Quit :: KeySym
k3270_Quit = 0xfd09
 
k3270_PA1 :: KeySym
k3270_PA1 = 0xfd0a
 
k3270_PA2 :: KeySym
k3270_PA2 = 0xfd0b
 
k3270_PA3 :: KeySym
k3270_PA3 = 0xfd0c
 
k3270_Test :: KeySym
k3270_Test = 0xfd0d
 
k3270_Attn :: KeySym
k3270_Attn = 0xfd0e
 
k3270_CursorBlink :: KeySym
k3270_CursorBlink = 0xfd0f
 
k3270_AltCursor :: KeySym
k3270_AltCursor = 0xfd10
 
k3270_KeyClick :: KeySym
k3270_KeyClick = 0xfd11
 
k3270_Jump :: KeySym
k3270_Jump = 0xfd12
 
k3270_Ident :: KeySym
k3270_Ident = 0xfd13
 
k3270_Rule :: KeySym
k3270_Rule = 0xfd14
 
k3270_Copy :: KeySym
k3270_Copy = 0xfd15
 
k3270_Play :: KeySym
k3270_Play = 0xfd16
 
k3270_Setup :: KeySym
k3270_Setup = 0xfd17
 
k3270_Record :: KeySym
k3270_Record = 0xfd18
 
k3270_ChangeScreen :: KeySym
k3270_ChangeScreen = 0xfd19
 
k3270_DeleteWord :: KeySym
k3270_DeleteWord = 0xfd1a
 
k3270_ExSelect :: KeySym
k3270_ExSelect = 0xfd1b
 
k3270_CursorSelect :: KeySym
k3270_CursorSelect = 0xfd1c
 
k3270_PrintScreen :: KeySym
k3270_PrintScreen = 0xfd1d
 
k3270_Enter :: KeySym
k3270_Enter = 0xfd1e
 
kspace :: KeySym
kspace = 0x020
 
kexclam :: KeySym
kexclam = 0x021
 
kquotedbl :: KeySym
kquotedbl = 0x022
 
knumbersign :: KeySym
knumbersign = 0x023
 
kdollar :: KeySym
kdollar = 0x024
 
kpercent :: KeySym
kpercent = 0x025
 
kampersand :: KeySym
kampersand = 0x026
 
kapostrophe :: KeySym
kapostrophe = 0x027
 
kquoteright :: KeySym
kquoteright = 0x027
 
kparenleft :: KeySym
kparenleft = 0x028
 
kparenright :: KeySym
kparenright = 0x029
 
kasterisk :: KeySym
kasterisk = 0x02a
 
kplus :: KeySym
kplus = 0x02b
 
kcomma :: KeySym
kcomma = 0x02c
 
kminus :: KeySym
kminus = 0x02d
 
kperiod :: KeySym
kperiod = 0x02e
 
kslash :: KeySym
kslash = 0x02f
 
k0 :: KeySym
k0 = 0x030
 
k1 :: KeySym
k1 = 0x031
 
k2 :: KeySym
k2 = 0x032
 
k3 :: KeySym
k3 = 0x033
 
k4 :: KeySym
k4 = 0x034
 
k5 :: KeySym
k5 = 0x035
 
k6 :: KeySym
k6 = 0x036
 
k7 :: KeySym
k7 = 0x037
 
k8 :: KeySym
k8 = 0x038
 
k9 :: KeySym
k9 = 0x039
 
kcolon :: KeySym
kcolon = 0x03a
 
ksemicolon :: KeySym
ksemicolon = 0x03b
 
kless :: KeySym
kless = 0x03c
 
kequal :: KeySym
kequal = 0x03d
 
kgreater :: KeySym
kgreater = 0x03e
 
kquestion :: KeySym
kquestion = 0x03f
 
kat :: KeySym
kat = 0x040
 
kA :: KeySym
kA = 0x041
 
kB :: KeySym
kB = 0x042
 
kC :: KeySym
kC = 0x043
 
kD :: KeySym
kD = 0x044
 
kE :: KeySym
kE = 0x045
 
kF :: KeySym
kF = 0x046
 
kG :: KeySym
kG = 0x047
 
kH :: KeySym
kH = 0x048
 
kI :: KeySym
kI = 0x049
 
kJ :: KeySym
kJ = 0x04a
 
kK :: KeySym
kK = 0x04b
 
kL :: KeySym
kL = 0x04c
 
kM :: KeySym
kM = 0x04d
 
kN :: KeySym
kN = 0x04e
 
kO :: KeySym
kO = 0x04f
 
kP :: KeySym
kP = 0x050
 
kQ :: KeySym
kQ = 0x051
 
kR :: KeySym
kR = 0x052
 
kS :: KeySym
kS = 0x053
 
kT :: KeySym
kT = 0x054
 
kU :: KeySym
kU = 0x055
 
kV :: KeySym
kV = 0x056
 
kW :: KeySym
kW = 0x057
 
kX :: KeySym
kX = 0x058
 
kY :: KeySym
kY = 0x059
 
kZ :: KeySym
kZ = 0x05a
 
kbracketleft :: KeySym
kbracketleft = 0x05b
 
kbackslash :: KeySym
kbackslash = 0x05c
 
kbracketright :: KeySym
kbracketright = 0x05d
 
kasciicircum :: KeySym
kasciicircum = 0x05e
 
kunderscore :: KeySym
kunderscore = 0x05f
 
kgrave :: KeySym
kgrave = 0x060
 
kquoteleft :: KeySym
kquoteleft = 0x060
 
ka :: KeySym
ka = 0x061
 
kb :: KeySym
kb = 0x062
 
kc :: KeySym
kc = 0x063
 
kd :: KeySym
kd = 0x064
 
ke :: KeySym
ke = 0x065
 
kf :: KeySym
kf = 0x066
 
kg :: KeySym
kg = 0x067
 
kh :: KeySym
kh = 0x068
 
ki :: KeySym
ki = 0x069
 
kj :: KeySym
kj = 0x06a
 
kk :: KeySym
kk = 0x06b
 
kl :: KeySym
kl = 0x06c
 
km :: KeySym
km = 0x06d
 
kn :: KeySym
kn = 0x06e
 
ko :: KeySym
ko = 0x06f
 
kp :: KeySym
kp = 0x070
 
kq :: KeySym
kq = 0x071
 
kr :: KeySym
kr = 0x072
 
ks :: KeySym
ks = 0x073
 
kt :: KeySym
kt = 0x074
 
ku :: KeySym
ku = 0x075
 
kv :: KeySym
kv = 0x076
 
kw :: KeySym
kw = 0x077
 
kx :: KeySym
kx = 0x078
 
ky :: KeySym
ky = 0x079
 
kz :: KeySym
kz = 0x07a
 
kbraceleft :: KeySym
kbraceleft = 0x07b
 
kbar :: KeySym
kbar = 0x07c
 
kbraceright :: KeySym
kbraceright = 0x07d
 
kasciitilde :: KeySym
kasciitilde = 0x07e
 
knobreakspace :: KeySym
knobreakspace = 0x0a0
 
kexclamdown :: KeySym
kexclamdown = 0x0a1
 
kcent :: KeySym
kcent = 0x0a2
 
ksterling :: KeySym
ksterling = 0x0a3
 
kcurrency :: KeySym
kcurrency = 0x0a4
 
kyen :: KeySym
kyen = 0x0a5
 
kbrokenbar :: KeySym
kbrokenbar = 0x0a6
 
ksection :: KeySym
ksection = 0x0a7
 
kdiaeresis :: KeySym
kdiaeresis = 0x0a8
 
kcopyright :: KeySym
kcopyright = 0x0a9
 
kordfeminine :: KeySym
kordfeminine = 0x0aa
 
kguillemotleft :: KeySym
kguillemotleft = 0x0ab
 
knotsign :: KeySym
knotsign = 0x0ac
 
khyphen :: KeySym
khyphen = 0x0ad
 
kregistered :: KeySym
kregistered = 0x0ae
 
kmacron :: KeySym
kmacron = 0x0af
 
kdegree :: KeySym
kdegree = 0x0b0
 
kplusminus :: KeySym
kplusminus = 0x0b1
 
ktwosuperior :: KeySym
ktwosuperior = 0x0b2
 
kthreesuperior :: KeySym
kthreesuperior = 0x0b3
 
kacute :: KeySym
kacute = 0x0b4
 
kmu :: KeySym
kmu = 0x0b5
 
kparagraph :: KeySym
kparagraph = 0x0b6
 
kperiodcentered :: KeySym
kperiodcentered = 0x0b7
 
kcedilla :: KeySym
kcedilla = 0x0b8
 
konesuperior :: KeySym
konesuperior = 0x0b9
 
kmasculine :: KeySym
kmasculine = 0x0ba
 
kguillemotright :: KeySym
kguillemotright = 0x0bb
 
konequarter :: KeySym
konequarter = 0x0bc
 
konehalf :: KeySym
konehalf = 0x0bd
 
kthreequarters :: KeySym
kthreequarters = 0x0be
 
kquestiondown :: KeySym
kquestiondown = 0x0bf
 
kAgrave :: KeySym
kAgrave = 0x0c0
 
kAacute :: KeySym
kAacute = 0x0c1
 
kAcircumflex :: KeySym
kAcircumflex = 0x0c2
 
kAtilde :: KeySym
kAtilde = 0x0c3
 
kAdiaeresis :: KeySym
kAdiaeresis = 0x0c4
 
kAring :: KeySym
kAring = 0x0c5
 
kAE :: KeySym
kAE = 0x0c6
 
kCcedilla :: KeySym
kCcedilla = 0x0c7
 
kEgrave :: KeySym
kEgrave = 0x0c8
 
kEacute :: KeySym
kEacute = 0x0c9
 
kEcircumflex :: KeySym
kEcircumflex = 0x0ca
 
kEdiaeresis :: KeySym
kEdiaeresis = 0x0cb
 
kIgrave :: KeySym
kIgrave = 0x0cc
 
kIacute :: KeySym
kIacute = 0x0cd
 
kIcircumflex :: KeySym
kIcircumflex = 0x0ce
 
kIdiaeresis :: KeySym
kIdiaeresis = 0x0cf
 
kETH :: KeySym
kETH = 0x0d0
 
kEth :: KeySym
kEth = 0x0d0
 
kNtilde :: KeySym
kNtilde = 0x0d1
 
kOgrave :: KeySym
kOgrave = 0x0d2
 
kOacute :: KeySym
kOacute = 0x0d3
 
kOcircumflex :: KeySym
kOcircumflex = 0x0d4
 
kOtilde :: KeySym
kOtilde = 0x0d5
 
kOdiaeresis :: KeySym
kOdiaeresis = 0x0d6
 
kmultiply :: KeySym
kmultiply = 0x0d7
 
kOslash :: KeySym
kOslash = 0x0d8
 
kOoblique :: KeySym
kOoblique = 0x0d8
 
kUgrave :: KeySym
kUgrave = 0x0d9
 
kUacute :: KeySym
kUacute = 0x0da
 
kUcircumflex :: KeySym
kUcircumflex = 0x0db
 
kUdiaeresis :: KeySym
kUdiaeresis = 0x0dc
 
kYacute :: KeySym
kYacute = 0x0dd
 
kTHORN :: KeySym
kTHORN = 0x0de
 
kThorn :: KeySym
kThorn = 0x0de
 
kssharp :: KeySym
kssharp = 0x0df
 
kagrave :: KeySym
kagrave = 0x0e0
 
kaacute :: KeySym
kaacute = 0x0e1
 
kacircumflex :: KeySym
kacircumflex = 0x0e2
 
katilde :: KeySym
katilde = 0x0e3
 
kadiaeresis :: KeySym
kadiaeresis = 0x0e4
 
karing :: KeySym
karing = 0x0e5
 
kae :: KeySym
kae = 0x0e6
 
kccedilla :: KeySym
kccedilla = 0x0e7
 
kegrave :: KeySym
kegrave = 0x0e8
 
keacute :: KeySym
keacute = 0x0e9
 
kecircumflex :: KeySym
kecircumflex = 0x0ea
 
kediaeresis :: KeySym
kediaeresis = 0x0eb
 
kigrave :: KeySym
kigrave = 0x0ec
 
kiacute :: KeySym
kiacute = 0x0ed
 
kicircumflex :: KeySym
kicircumflex = 0x0ee
 
kidiaeresis :: KeySym
kidiaeresis = 0x0ef
 
keth :: KeySym
keth = 0x0f0
 
kntilde :: KeySym
kntilde = 0x0f1
 
kograve :: KeySym
kograve = 0x0f2
 
koacute :: KeySym
koacute = 0x0f3
 
kocircumflex :: KeySym
kocircumflex = 0x0f4
 
kotilde :: KeySym
kotilde = 0x0f5
 
kodiaeresis :: KeySym
kodiaeresis = 0x0f6
 
kdivision :: KeySym
kdivision = 0x0f7
 
koslash :: KeySym
koslash = 0x0f8
 
kooblique :: KeySym
kooblique = 0x0f8
 
kugrave :: KeySym
kugrave = 0x0f9
 
kuacute :: KeySym
kuacute = 0x0fa
 
kucircumflex :: KeySym
kucircumflex = 0x0fb
 
kudiaeresis :: KeySym
kudiaeresis = 0x0fc
 
kyacute :: KeySym
kyacute = 0x0fd
 
kthorn :: KeySym
kthorn = 0x0fe
 
kydiaeresis :: KeySym
kydiaeresis = 0x0ff
 
kAogonek :: KeySym
kAogonek = 0x1a1
 
kbreve :: KeySym
kbreve = 0x1a2
 
kLstroke :: KeySym
kLstroke = 0x1a3
 
kLcaron :: KeySym
kLcaron = 0x1a5
 
kSacute :: KeySym
kSacute = 0x1a6
 
kScaron :: KeySym
kScaron = 0x1a9
 
kScedilla :: KeySym
kScedilla = 0x1aa
 
kTcaron :: KeySym
kTcaron = 0x1ab
 
kZacute :: KeySym
kZacute = 0x1ac
 
kZcaron :: KeySym
kZcaron = 0x1ae
 
kZabovedot :: KeySym
kZabovedot = 0x1af
 
kaogonek :: KeySym
kaogonek = 0x1b1
 
kogonek :: KeySym
kogonek = 0x1b2
 
klstroke :: KeySym
klstroke = 0x1b3
 
klcaron :: KeySym
klcaron = 0x1b5
 
ksacute :: KeySym
ksacute = 0x1b6
 
kcaron :: KeySym
kcaron = 0x1b7
 
kscaron :: KeySym
kscaron = 0x1b9
 
kscedilla :: KeySym
kscedilla = 0x1ba
 
ktcaron :: KeySym
ktcaron = 0x1bb
 
kzacute :: KeySym
kzacute = 0x1bc
 
kdoubleacute :: KeySym
kdoubleacute = 0x1bd
 
kzcaron :: KeySym
kzcaron = 0x1be
 
kzabovedot :: KeySym
kzabovedot = 0x1bf
 
kRacute :: KeySym
kRacute = 0x1c0
 
kAbreve :: KeySym
kAbreve = 0x1c3
 
kLacute :: KeySym
kLacute = 0x1c5
 
kCacute :: KeySym
kCacute = 0x1c6
 
kCcaron :: KeySym
kCcaron = 0x1c8
 
kEogonek :: KeySym
kEogonek = 0x1ca
 
kEcaron :: KeySym
kEcaron = 0x1cc
 
kDcaron :: KeySym
kDcaron = 0x1cf
 
kDstroke :: KeySym
kDstroke = 0x1d0
 
kNacute :: KeySym
kNacute = 0x1d1
 
kNcaron :: KeySym
kNcaron = 0x1d2
 
kOdoubleacute :: KeySym
kOdoubleacute = 0x1d5
 
kRcaron :: KeySym
kRcaron = 0x1d8
 
kUring :: KeySym
kUring = 0x1d9
 
kUdoubleacute :: KeySym
kUdoubleacute = 0x1db
 
kTcedilla :: KeySym
kTcedilla = 0x1de
 
kracute :: KeySym
kracute = 0x1e0
 
kabreve :: KeySym
kabreve = 0x1e3
 
klacute :: KeySym
klacute = 0x1e5
 
kcacute :: KeySym
kcacute = 0x1e6
 
kccaron :: KeySym
kccaron = 0x1e8
 
keogonek :: KeySym
keogonek = 0x1ea
 
kecaron :: KeySym
kecaron = 0x1ec
 
kdcaron :: KeySym
kdcaron = 0x1ef
 
kdstroke :: KeySym
kdstroke = 0x1f0
 
knacute :: KeySym
knacute = 0x1f1
 
kncaron :: KeySym
kncaron = 0x1f2
 
kodoubleacute :: KeySym
kodoubleacute = 0x1f5
 
kudoubleacute :: KeySym
kudoubleacute = 0x1fb
 
krcaron :: KeySym
krcaron = 0x1f8
 
kuring :: KeySym
kuring = 0x1f9
 
ktcedilla :: KeySym
ktcedilla = 0x1fe
 
kabovedot :: KeySym
kabovedot = 0x1ff
 
kHstroke :: KeySym
kHstroke = 0x2a1
 
kHcircumflex :: KeySym
kHcircumflex = 0x2a6
 
kIabovedot :: KeySym
kIabovedot = 0x2a9
 
kGbreve :: KeySym
kGbreve = 0x2ab
 
kJcircumflex :: KeySym
kJcircumflex = 0x2ac
 
khstroke :: KeySym
khstroke = 0x2b1
 
khcircumflex :: KeySym
khcircumflex = 0x2b6
 
kidotless :: KeySym
kidotless = 0x2b9
 
kgbreve :: KeySym
kgbreve = 0x2bb
 
kjcircumflex :: KeySym
kjcircumflex = 0x2bc
 
kCabovedot :: KeySym
kCabovedot = 0x2c5
 
kCcircumflex :: KeySym
kCcircumflex = 0x2c6
 
kGabovedot :: KeySym
kGabovedot = 0x2d5
 
kGcircumflex :: KeySym
kGcircumflex = 0x2d8
 
kUbreve :: KeySym
kUbreve = 0x2dd
 
kScircumflex :: KeySym
kScircumflex = 0x2de
 
kcabovedot :: KeySym
kcabovedot = 0x2e5
 
kccircumflex :: KeySym
kccircumflex = 0x2e6
 
kgabovedot :: KeySym
kgabovedot = 0x2f5
 
kgcircumflex :: KeySym
kgcircumflex = 0x2f8
 
kubreve :: KeySym
kubreve = 0x2fd
 
kscircumflex :: KeySym
kscircumflex = 0x2fe
 
kkra :: KeySym
kkra = 0x3a2
 
kkappa :: KeySym
kkappa = 0x3a2
 
kRcedilla :: KeySym
kRcedilla = 0x3a3
 
kItilde :: KeySym
kItilde = 0x3a5
 
kLcedilla :: KeySym
kLcedilla = 0x3a6
 
kEmacron :: KeySym
kEmacron = 0x3aa
 
kGcedilla :: KeySym
kGcedilla = 0x3ab
 
kTslash :: KeySym
kTslash = 0x3ac
 
krcedilla :: KeySym
krcedilla = 0x3b3
 
kitilde :: KeySym
kitilde = 0x3b5
 
klcedilla :: KeySym
klcedilla = 0x3b6
 
kemacron :: KeySym
kemacron = 0x3ba
 
kgcedilla :: KeySym
kgcedilla = 0x3bb
 
ktslash :: KeySym
ktslash = 0x3bc
 
kENG :: KeySym
kENG = 0x3bd
 
keng :: KeySym
keng = 0x3bf
 
kAmacron :: KeySym
kAmacron = 0x3c0
 
kIogonek :: KeySym
kIogonek = 0x3c7
 
kEabovedot :: KeySym
kEabovedot = 0x3cc
 
kImacron :: KeySym
kImacron = 0x3cf
 
kNcedilla :: KeySym
kNcedilla = 0x3d1
 
kOmacron :: KeySym
kOmacron = 0x3d2
 
kKcedilla :: KeySym
kKcedilla = 0x3d3
 
kUogonek :: KeySym
kUogonek = 0x3d9
 
kUtilde :: KeySym
kUtilde = 0x3dd
 
kUmacron :: KeySym
kUmacron = 0x3de
 
kamacron :: KeySym
kamacron = 0x3e0
 
kiogonek :: KeySym
kiogonek = 0x3e7
 
keabovedot :: KeySym
keabovedot = 0x3ec
 
kimacron :: KeySym
kimacron = 0x3ef
 
kncedilla :: KeySym
kncedilla = 0x3f1
 
komacron :: KeySym
komacron = 0x3f2
 
kkcedilla :: KeySym
kkcedilla = 0x3f3
 
kuogonek :: KeySym
kuogonek = 0x3f9
 
kutilde :: KeySym
kutilde = 0x3fd
 
kumacron :: KeySym
kumacron = 0x3fe
 
kBabovedot :: KeySym
kBabovedot = 0x1001e02
 
kbabovedot :: KeySym
kbabovedot = 0x1001e03
 
kDabovedot :: KeySym
kDabovedot = 0x1001e0a
 
kWgrave :: KeySym
kWgrave = 0x1001e80
 
kWacute :: KeySym
kWacute = 0x1001e82
 
kdabovedot :: KeySym
kdabovedot = 0x1001e0b
 
kYgrave :: KeySym
kYgrave = 0x1001ef2
 
kFabovedot :: KeySym
kFabovedot = 0x1001e1e
 
kfabovedot :: KeySym
kfabovedot = 0x1001e1f
 
kMabovedot :: KeySym
kMabovedot = 0x1001e40
 
kmabovedot :: KeySym
kmabovedot = 0x1001e41
 
kPabovedot :: KeySym
kPabovedot = 0x1001e56
 
kwgrave :: KeySym
kwgrave = 0x1001e81
 
kpabovedot :: KeySym
kpabovedot = 0x1001e57
 
kwacute :: KeySym
kwacute = 0x1001e83
 
kSabovedot :: KeySym
kSabovedot = 0x1001e60
 
kygrave :: KeySym
kygrave = 0x1001ef3
 
kWdiaeresis :: KeySym
kWdiaeresis = 0x1001e84
 
kwdiaeresis :: KeySym
kwdiaeresis = 0x1001e85
 
ksabovedot :: KeySym
ksabovedot = 0x1001e61
 
kWcircumflex :: KeySym
kWcircumflex = 0x1000174
 
kTabovedot :: KeySym
kTabovedot = 0x1001e6a
 
kYcircumflex :: KeySym
kYcircumflex = 0x1000176
 
kwcircumflex :: KeySym
kwcircumflex = 0x1000175
 
ktabovedot :: KeySym
ktabovedot = 0x1001e6b
 
kycircumflex :: KeySym
kycircumflex = 0x1000177
 
kOE :: KeySym
kOE = 0x13bc
 
koe :: KeySym
koe = 0x13bd
 
kYdiaeresis :: KeySym
kYdiaeresis = 0x13be
 
koverline :: KeySym
koverline = 0x47e
 
kkana_fullstop :: KeySym
kkana_fullstop = 0x4a1
 
kkana_openingbracket :: KeySym
kkana_openingbracket = 0x4a2
 
kkana_closingbracket :: KeySym
kkana_closingbracket = 0x4a3
 
kkana_comma :: KeySym
kkana_comma = 0x4a4
 
kkana_conjunctive :: KeySym
kkana_conjunctive = 0x4a5
 
kkana_middledot :: KeySym
kkana_middledot = 0x4a5
 
kkana_WO :: KeySym
kkana_WO = 0x4a6
 
kkana_a :: KeySym
kkana_a = 0x4a7
 
kkana_i :: KeySym
kkana_i = 0x4a8
 
kkana_u :: KeySym
kkana_u = 0x4a9
 
kkana_e :: KeySym
kkana_e = 0x4aa
 
kkana_o :: KeySym
kkana_o = 0x4ab
 
kkana_ya :: KeySym
kkana_ya = 0x4ac
 
kkana_yu :: KeySym
kkana_yu = 0x4ad
 
kkana_yo :: KeySym
kkana_yo = 0x4ae
 
kkana_tsu :: KeySym
kkana_tsu = 0x4af
 
kkana_tu :: KeySym
kkana_tu = 0x4af
 
kprolongedsound :: KeySym
kprolongedsound = 0x4b0
 
kkana_A :: KeySym
kkana_A = 0x4b1
 
kkana_I :: KeySym
kkana_I = 0x4b2
 
kkana_U :: KeySym
kkana_U = 0x4b3
 
kkana_E :: KeySym
kkana_E = 0x4b4
 
kkana_O :: KeySym
kkana_O = 0x4b5
 
kkana_KA :: KeySym
kkana_KA = 0x4b6
 
kkana_KI :: KeySym
kkana_KI = 0x4b7
 
kkana_KU :: KeySym
kkana_KU = 0x4b8
 
kkana_KE :: KeySym
kkana_KE = 0x4b9
 
kkana_KO :: KeySym
kkana_KO = 0x4ba
 
kkana_SA :: KeySym
kkana_SA = 0x4bb
 
kkana_SHI :: KeySym
kkana_SHI = 0x4bc
 
kkana_SU :: KeySym
kkana_SU = 0x4bd
 
kkana_SE :: KeySym
kkana_SE = 0x4be
 
kkana_SO :: KeySym
kkana_SO = 0x4bf
 
kkana_TA :: KeySym
kkana_TA = 0x4c0
 
kkana_CHI :: KeySym
kkana_CHI = 0x4c1
 
kkana_TI :: KeySym
kkana_TI = 0x4c1
 
kkana_TSU :: KeySym
kkana_TSU = 0x4c2
 
kkana_TU :: KeySym
kkana_TU = 0x4c2
 
kkana_TE :: KeySym
kkana_TE = 0x4c3
 
kkana_TO :: KeySym
kkana_TO = 0x4c4
 
kkana_NA :: KeySym
kkana_NA = 0x4c5
 
kkana_NI :: KeySym
kkana_NI = 0x4c6
 
kkana_NU :: KeySym
kkana_NU = 0x4c7
 
kkana_NE :: KeySym
kkana_NE = 0x4c8
 
kkana_NO :: KeySym
kkana_NO = 0x4c9
 
kkana_HA :: KeySym
kkana_HA = 0x4ca
 
kkana_HI :: KeySym
kkana_HI = 0x4cb
 
kkana_FU :: KeySym
kkana_FU = 0x4cc
 
kkana_HU :: KeySym
kkana_HU = 0x4cc
 
kkana_HE :: KeySym
kkana_HE = 0x4cd
 
kkana_HO :: KeySym
kkana_HO = 0x4ce
 
kkana_MA :: KeySym
kkana_MA = 0x4cf
 
kkana_MI :: KeySym
kkana_MI = 0x4d0
 
kkana_MU :: KeySym
kkana_MU = 0x4d1
 
kkana_ME :: KeySym
kkana_ME = 0x4d2
 
kkana_MO :: KeySym
kkana_MO = 0x4d3
 
kkana_YA :: KeySym
kkana_YA = 0x4d4
 
kkana_YU :: KeySym
kkana_YU = 0x4d5
 
kkana_YO :: KeySym
kkana_YO = 0x4d6
 
kkana_RA :: KeySym
kkana_RA = 0x4d7
 
kkana_RI :: KeySym
kkana_RI = 0x4d8
 
kkana_RU :: KeySym
kkana_RU = 0x4d9
 
kkana_RE :: KeySym
kkana_RE = 0x4da
 
kkana_RO :: KeySym
kkana_RO = 0x4db
 
kkana_WA :: KeySym
kkana_WA = 0x4dc
 
kkana_N :: KeySym
kkana_N = 0x4dd
 
kvoicedsound :: KeySym
kvoicedsound = 0x4de
 
ksemivoicedsound :: KeySym
ksemivoicedsound = 0x4df
 
kkana_switch :: KeySym
kkana_switch = 0xff7e
 
kFarsi_0 :: KeySym
kFarsi_0 = 0x10006f0
 
kFarsi_1 :: KeySym
kFarsi_1 = 0x10006f1
 
kFarsi_2 :: KeySym
kFarsi_2 = 0x10006f2
 
kFarsi_3 :: KeySym
kFarsi_3 = 0x10006f3
 
kFarsi_4 :: KeySym
kFarsi_4 = 0x10006f4
 
kFarsi_5 :: KeySym
kFarsi_5 = 0x10006f5
 
kFarsi_6 :: KeySym
kFarsi_6 = 0x10006f6
 
kFarsi_7 :: KeySym
kFarsi_7 = 0x10006f7
 
kFarsi_8 :: KeySym
kFarsi_8 = 0x10006f8
 
kFarsi_9 :: KeySym
kFarsi_9 = 0x10006f9
 
kArabic_percent :: KeySym
kArabic_percent = 0x100066a
 
kArabic_superscript_alef :: KeySym
kArabic_superscript_alef = 0x1000670
 
kArabic_tteh :: KeySym
kArabic_tteh = 0x1000679
 
kArabic_peh :: KeySym
kArabic_peh = 0x100067e
 
kArabic_tcheh :: KeySym
kArabic_tcheh = 0x1000686
 
kArabic_ddal :: KeySym
kArabic_ddal = 0x1000688
 
kArabic_rreh :: KeySym
kArabic_rreh = 0x1000691
 
kArabic_comma :: KeySym
kArabic_comma = 0x5ac
 
kArabic_fullstop :: KeySym
kArabic_fullstop = 0x10006d4
 
kArabic_0 :: KeySym
kArabic_0 = 0x1000660
 
kArabic_1 :: KeySym
kArabic_1 = 0x1000661
 
kArabic_2 :: KeySym
kArabic_2 = 0x1000662
 
kArabic_3 :: KeySym
kArabic_3 = 0x1000663
 
kArabic_4 :: KeySym
kArabic_4 = 0x1000664
 
kArabic_5 :: KeySym
kArabic_5 = 0x1000665
 
kArabic_6 :: KeySym
kArabic_6 = 0x1000666
 
kArabic_7 :: KeySym
kArabic_7 = 0x1000667
 
kArabic_8 :: KeySym
kArabic_8 = 0x1000668
 
kArabic_9 :: KeySym
kArabic_9 = 0x1000669
 
kArabic_semicolon :: KeySym
kArabic_semicolon = 0x5bb
 
kArabic_question_mark :: KeySym
kArabic_question_mark = 0x5bf
 
kArabic_hamza :: KeySym
kArabic_hamza = 0x5c1
 
kArabic_maddaonalef :: KeySym
kArabic_maddaonalef = 0x5c2
 
kArabic_hamzaonalef :: KeySym
kArabic_hamzaonalef = 0x5c3
 
kArabic_hamzaonwaw :: KeySym
kArabic_hamzaonwaw = 0x5c4
 
kArabic_hamzaunderalef :: KeySym
kArabic_hamzaunderalef = 0x5c5
 
kArabic_hamzaonyeh :: KeySym
kArabic_hamzaonyeh = 0x5c6
 
kArabic_alef :: KeySym
kArabic_alef = 0x5c7
 
kArabic_beh :: KeySym
kArabic_beh = 0x5c8
 
kArabic_tehmarbuta :: KeySym
kArabic_tehmarbuta = 0x5c9
 
kArabic_teh :: KeySym
kArabic_teh = 0x5ca
 
kArabic_theh :: KeySym
kArabic_theh = 0x5cb
 
kArabic_jeem :: KeySym
kArabic_jeem = 0x5cc
 
kArabic_hah :: KeySym
kArabic_hah = 0x5cd
 
kArabic_khah :: KeySym
kArabic_khah = 0x5ce
 
kArabic_dal :: KeySym
kArabic_dal = 0x5cf
 
kArabic_thal :: KeySym
kArabic_thal = 0x5d0
 
kArabic_ra :: KeySym
kArabic_ra = 0x5d1
 
kArabic_zain :: KeySym
kArabic_zain = 0x5d2
 
kArabic_seen :: KeySym
kArabic_seen = 0x5d3
 
kArabic_sheen :: KeySym
kArabic_sheen = 0x5d4
 
kArabic_sad :: KeySym
kArabic_sad = 0x5d5
 
kArabic_dad :: KeySym
kArabic_dad = 0x5d6
 
kArabic_tah :: KeySym
kArabic_tah = 0x5d7
 
kArabic_zah :: KeySym
kArabic_zah = 0x5d8
 
kArabic_ain :: KeySym
kArabic_ain = 0x5d9
 
kArabic_ghain :: KeySym
kArabic_ghain = 0x5da
 
kArabic_tatweel :: KeySym
kArabic_tatweel = 0x5e0
 
kArabic_feh :: KeySym
kArabic_feh = 0x5e1
 
kArabic_qaf :: KeySym
kArabic_qaf = 0x5e2
 
kArabic_kaf :: KeySym
kArabic_kaf = 0x5e3
 
kArabic_lam :: KeySym
kArabic_lam = 0x5e4
 
kArabic_meem :: KeySym
kArabic_meem = 0x5e5
 
kArabic_noon :: KeySym
kArabic_noon = 0x5e6
 
kArabic_ha :: KeySym
kArabic_ha = 0x5e7
 
kArabic_heh :: KeySym
kArabic_heh = 0x5e7
 
kArabic_waw :: KeySym
kArabic_waw = 0x5e8
 
kArabic_alefmaksura :: KeySym
kArabic_alefmaksura = 0x5e9
 
kArabic_yeh :: KeySym
kArabic_yeh = 0x5ea
 
kArabic_fathatan :: KeySym
kArabic_fathatan = 0x5eb
 
kArabic_dammatan :: KeySym
kArabic_dammatan = 0x5ec
 
kArabic_kasratan :: KeySym
kArabic_kasratan = 0x5ed
 
kArabic_fatha :: KeySym
kArabic_fatha = 0x5ee
 
kArabic_damma :: KeySym
kArabic_damma = 0x5ef
 
kArabic_kasra :: KeySym
kArabic_kasra = 0x5f0
 
kArabic_shadda :: KeySym
kArabic_shadda = 0x5f1
 
kArabic_sukun :: KeySym
kArabic_sukun = 0x5f2
 
kArabic_madda_above :: KeySym
kArabic_madda_above = 0x1000653
 
kArabic_hamza_above :: KeySym
kArabic_hamza_above = 0x1000654
 
kArabic_hamza_below :: KeySym
kArabic_hamza_below = 0x1000655
 
kArabic_jeh :: KeySym
kArabic_jeh = 0x1000698
 
kArabic_veh :: KeySym
kArabic_veh = 0x10006a4
 
kArabic_keheh :: KeySym
kArabic_keheh = 0x10006a9
 
kArabic_gaf :: KeySym
kArabic_gaf = 0x10006af
 
kArabic_noon_ghunna :: KeySym
kArabic_noon_ghunna = 0x10006ba
 
kArabic_heh_doachashmee :: KeySym
kArabic_heh_doachashmee = 0x10006be
 
kFarsi_yeh :: KeySym
kFarsi_yeh = 0x10006cc
 
kArabic_farsi_yeh :: KeySym
kArabic_farsi_yeh = 0x10006cc
 
kArabic_yeh_baree :: KeySym
kArabic_yeh_baree = 0x10006d2
 
kArabic_heh_goal :: KeySym
kArabic_heh_goal = 0x10006c1
 
kArabic_switch :: KeySym
kArabic_switch = 0xff7e
 
kCyrillic_GHE_bar :: KeySym
kCyrillic_GHE_bar = 0x1000492
 
kCyrillic_ghe_bar :: KeySym
kCyrillic_ghe_bar = 0x1000493
 
kCyrillic_ZHE_descender :: KeySym
kCyrillic_ZHE_descender = 0x1000496
 
kCyrillic_zhe_descender :: KeySym
kCyrillic_zhe_descender = 0x1000497
 
kCyrillic_KA_descender :: KeySym
kCyrillic_KA_descender = 0x100049a
 
kCyrillic_ka_descender :: KeySym
kCyrillic_ka_descender = 0x100049b
 
kCyrillic_KA_vertstroke :: KeySym
kCyrillic_KA_vertstroke = 0x100049c
 
kCyrillic_ka_vertstroke :: KeySym
kCyrillic_ka_vertstroke = 0x100049d
 
kCyrillic_EN_descender :: KeySym
kCyrillic_EN_descender = 0x10004a2
 
kCyrillic_en_descender :: KeySym
kCyrillic_en_descender = 0x10004a3
 
kCyrillic_U_straight :: KeySym
kCyrillic_U_straight = 0x10004ae
 
kCyrillic_u_straight :: KeySym
kCyrillic_u_straight = 0x10004af
 
kCyrillic_U_straight_bar :: KeySym
kCyrillic_U_straight_bar = 0x10004b0
 
kCyrillic_u_straight_bar :: KeySym
kCyrillic_u_straight_bar = 0x10004b1
 
kCyrillic_HA_descender :: KeySym
kCyrillic_HA_descender = 0x10004b2
 
kCyrillic_ha_descender :: KeySym
kCyrillic_ha_descender = 0x10004b3
 
kCyrillic_CHE_descender :: KeySym
kCyrillic_CHE_descender = 0x10004b6
 
kCyrillic_che_descender :: KeySym
kCyrillic_che_descender = 0x10004b7
 
kCyrillic_CHE_vertstroke :: KeySym
kCyrillic_CHE_vertstroke = 0x10004b8
 
kCyrillic_che_vertstroke :: KeySym
kCyrillic_che_vertstroke = 0x10004b9
 
kCyrillic_SHHA :: KeySym
kCyrillic_SHHA = 0x10004ba
 
kCyrillic_shha :: KeySym
kCyrillic_shha = 0x10004bb
 
kCyrillic_SCHWA :: KeySym
kCyrillic_SCHWA = 0x10004d8
 
kCyrillic_schwa :: KeySym
kCyrillic_schwa = 0x10004d9
 
kCyrillic_I_macron :: KeySym
kCyrillic_I_macron = 0x10004e2
 
kCyrillic_i_macron :: KeySym
kCyrillic_i_macron = 0x10004e3
 
kCyrillic_O_bar :: KeySym
kCyrillic_O_bar = 0x10004e8
 
kCyrillic_o_bar :: KeySym
kCyrillic_o_bar = 0x10004e9
 
kCyrillic_U_macron :: KeySym
kCyrillic_U_macron = 0x10004ee
 
kCyrillic_u_macron :: KeySym
kCyrillic_u_macron = 0x10004ef
 
kSerbian_dje :: KeySym
kSerbian_dje = 0x6a1
 
kMacedonia_gje :: KeySym
kMacedonia_gje = 0x6a2
 
kCyrillic_io :: KeySym
kCyrillic_io = 0x6a3
 
kUkrainian_ie :: KeySym
kUkrainian_ie = 0x6a4
 
kUkranian_je :: KeySym
kUkranian_je = 0x6a4
 
kMacedonia_dse :: KeySym
kMacedonia_dse = 0x6a5
 
kUkrainian_i :: KeySym
kUkrainian_i = 0x6a6
 
kUkranian_i :: KeySym
kUkranian_i = 0x6a6
 
kUkrainian_yi :: KeySym
kUkrainian_yi = 0x6a7
 
kUkranian_yi :: KeySym
kUkranian_yi = 0x6a7
 
kCyrillic_je :: KeySym
kCyrillic_je = 0x6a8
 
kSerbian_je :: KeySym
kSerbian_je = 0x6a8
 
kCyrillic_lje :: KeySym
kCyrillic_lje = 0x6a9
 
kSerbian_lje :: KeySym
kSerbian_lje = 0x6a9
 
kCyrillic_nje :: KeySym
kCyrillic_nje = 0x6aa
 
kSerbian_nje :: KeySym
kSerbian_nje = 0x6aa
 
kSerbian_tshe :: KeySym
kSerbian_tshe = 0x6ab
 
kMacedonia_kje :: KeySym
kMacedonia_kje = 0x6ac
 
kUkrainian_ghe_with_upturn :: KeySym
kUkrainian_ghe_with_upturn = 0x6ad
 
kByelorussian_shortu :: KeySym
kByelorussian_shortu = 0x6ae
 
kCyrillic_dzhe :: KeySym
kCyrillic_dzhe = 0x6af
 
kSerbian_dze :: KeySym
kSerbian_dze = 0x6af
 
knumerosign :: KeySym
knumerosign = 0x6b0
 
kSerbian_DJE :: KeySym
kSerbian_DJE = 0x6b1
 
kMacedonia_GJE :: KeySym
kMacedonia_GJE = 0x6b2
 
kCyrillic_IO :: KeySym
kCyrillic_IO = 0x6b3
 
kUkrainian_IE :: KeySym
kUkrainian_IE = 0x6b4
 
kUkranian_JE :: KeySym
kUkranian_JE = 0x6b4
 
kMacedonia_DSE :: KeySym
kMacedonia_DSE = 0x6b5
 
kUkrainian_I :: KeySym
kUkrainian_I = 0x6b6
 
kUkranian_I :: KeySym
kUkranian_I = 0x6b6
 
kUkrainian_YI :: KeySym
kUkrainian_YI = 0x6b7
 
kUkranian_YI :: KeySym
kUkranian_YI = 0x6b7
 
kCyrillic_JE :: KeySym
kCyrillic_JE = 0x6b8
 
kSerbian_JE :: KeySym
kSerbian_JE = 0x6b8
 
kCyrillic_LJE :: KeySym
kCyrillic_LJE = 0x6b9
 
kSerbian_LJE :: KeySym
kSerbian_LJE = 0x6b9
 
kCyrillic_NJE :: KeySym
kCyrillic_NJE = 0x6ba
 
kSerbian_NJE :: KeySym
kSerbian_NJE = 0x6ba
 
kSerbian_TSHE :: KeySym
kSerbian_TSHE = 0x6bb
 
kMacedonia_KJE :: KeySym
kMacedonia_KJE = 0x6bc
 
kUkrainian_GHE_WITH_UPTURN :: KeySym
kUkrainian_GHE_WITH_UPTURN = 0x6bd
 
kByelorussian_SHORTU :: KeySym
kByelorussian_SHORTU = 0x6be
 
kCyrillic_DZHE :: KeySym
kCyrillic_DZHE = 0x6bf
 
kSerbian_DZE :: KeySym
kSerbian_DZE = 0x6bf
 
kCyrillic_yu :: KeySym
kCyrillic_yu = 0x6c0
 
kCyrillic_a :: KeySym
kCyrillic_a = 0x6c1
 
kCyrillic_be :: KeySym
kCyrillic_be = 0x6c2
 
kCyrillic_tse :: KeySym
kCyrillic_tse = 0x6c3
 
kCyrillic_de :: KeySym
kCyrillic_de = 0x6c4
 
kCyrillic_ie :: KeySym
kCyrillic_ie = 0x6c5
 
kCyrillic_ef :: KeySym
kCyrillic_ef = 0x6c6
 
kCyrillic_ghe :: KeySym
kCyrillic_ghe = 0x6c7
 
kCyrillic_ha :: KeySym
kCyrillic_ha = 0x6c8
 
kCyrillic_i :: KeySym
kCyrillic_i = 0x6c9
 
kCyrillic_shorti :: KeySym
kCyrillic_shorti = 0x6ca
 
kCyrillic_ka :: KeySym
kCyrillic_ka = 0x6cb
 
kCyrillic_el :: KeySym
kCyrillic_el = 0x6cc
 
kCyrillic_em :: KeySym
kCyrillic_em = 0x6cd
 
kCyrillic_en :: KeySym
kCyrillic_en = 0x6ce
 
kCyrillic_o :: KeySym
kCyrillic_o = 0x6cf
 
kCyrillic_pe :: KeySym
kCyrillic_pe = 0x6d0
 
kCyrillic_ya :: KeySym
kCyrillic_ya = 0x6d1
 
kCyrillic_er :: KeySym
kCyrillic_er = 0x6d2
 
kCyrillic_es :: KeySym
kCyrillic_es = 0x6d3
 
kCyrillic_te :: KeySym
kCyrillic_te = 0x6d4
 
kCyrillic_u :: KeySym
kCyrillic_u = 0x6d5
 
kCyrillic_zhe :: KeySym
kCyrillic_zhe = 0x6d6
 
kCyrillic_ve :: KeySym
kCyrillic_ve = 0x6d7
 
kCyrillic_softsign :: KeySym
kCyrillic_softsign = 0x6d8
 
kCyrillic_yeru :: KeySym
kCyrillic_yeru = 0x6d9
 
kCyrillic_ze :: KeySym
kCyrillic_ze = 0x6da
 
kCyrillic_sha :: KeySym
kCyrillic_sha = 0x6db
 
kCyrillic_e :: KeySym
kCyrillic_e = 0x6dc
 
kCyrillic_shcha :: KeySym
kCyrillic_shcha = 0x6dd
 
kCyrillic_che :: KeySym
kCyrillic_che = 0x6de
 
kCyrillic_hardsign :: KeySym
kCyrillic_hardsign = 0x6df
 
kCyrillic_YU :: KeySym
kCyrillic_YU = 0x6e0
 
kCyrillic_A :: KeySym
kCyrillic_A = 0x6e1
 
kCyrillic_BE :: KeySym
kCyrillic_BE = 0x6e2
 
kCyrillic_TSE :: KeySym
kCyrillic_TSE = 0x6e3
 
kCyrillic_DE :: KeySym
kCyrillic_DE = 0x6e4
 
kCyrillic_IE :: KeySym
kCyrillic_IE = 0x6e5
 
kCyrillic_EF :: KeySym
kCyrillic_EF = 0x6e6
 
kCyrillic_GHE :: KeySym
kCyrillic_GHE = 0x6e7
 
kCyrillic_HA :: KeySym
kCyrillic_HA = 0x6e8
 
kCyrillic_I :: KeySym
kCyrillic_I = 0x6e9
 
kCyrillic_SHORTI :: KeySym
kCyrillic_SHORTI = 0x6ea
 
kCyrillic_KA :: KeySym
kCyrillic_KA = 0x6eb
 
kCyrillic_EL :: KeySym
kCyrillic_EL = 0x6ec
 
kCyrillic_EM :: KeySym
kCyrillic_EM = 0x6ed
 
kCyrillic_EN :: KeySym
kCyrillic_EN = 0x6ee
 
kCyrillic_O :: KeySym
kCyrillic_O = 0x6ef
 
kCyrillic_PE :: KeySym
kCyrillic_PE = 0x6f0
 
kCyrillic_YA :: KeySym
kCyrillic_YA = 0x6f1
 
kCyrillic_ER :: KeySym
kCyrillic_ER = 0x6f2
 
kCyrillic_ES :: KeySym
kCyrillic_ES = 0x6f3
 
kCyrillic_TE :: KeySym
kCyrillic_TE = 0x6f4
 
kCyrillic_U :: KeySym
kCyrillic_U = 0x6f5
 
kCyrillic_ZHE :: KeySym
kCyrillic_ZHE = 0x6f6
 
kCyrillic_VE :: KeySym
kCyrillic_VE = 0x6f7
 
kCyrillic_SOFTSIGN :: KeySym
kCyrillic_SOFTSIGN = 0x6f8
 
kCyrillic_YERU :: KeySym
kCyrillic_YERU = 0x6f9
 
kCyrillic_ZE :: KeySym
kCyrillic_ZE = 0x6fa
 
kCyrillic_SHA :: KeySym
kCyrillic_SHA = 0x6fb
 
kCyrillic_E :: KeySym
kCyrillic_E = 0x6fc
 
kCyrillic_SHCHA :: KeySym
kCyrillic_SHCHA = 0x6fd
 
kCyrillic_CHE :: KeySym
kCyrillic_CHE = 0x6fe
 
kCyrillic_HARDSIGN :: KeySym
kCyrillic_HARDSIGN = 0x6ff
 
kGreek_ALPHAaccent :: KeySym
kGreek_ALPHAaccent = 0x7a1
 
kGreek_EPSILONaccent :: KeySym
kGreek_EPSILONaccent = 0x7a2
 
kGreek_ETAaccent :: KeySym
kGreek_ETAaccent = 0x7a3
 
kGreek_IOTAaccent :: KeySym
kGreek_IOTAaccent = 0x7a4
 
kGreek_IOTAdieresis :: KeySym
kGreek_IOTAdieresis = 0x7a5
 
kGreek_IOTAdiaeresis :: KeySym
kGreek_IOTAdiaeresis = 0x7a5
 
kGreek_OMICRONaccent :: KeySym
kGreek_OMICRONaccent = 0x7a7
 
kGreek_UPSILONaccent :: KeySym
kGreek_UPSILONaccent = 0x7a8
 
kGreek_UPSILONdieresis :: KeySym
kGreek_UPSILONdieresis = 0x7a9
 
kGreek_OMEGAaccent :: KeySym
kGreek_OMEGAaccent = 0x7ab
 
kGreek_accentdieresis :: KeySym
kGreek_accentdieresis = 0x7ae
 
kGreek_horizbar :: KeySym
kGreek_horizbar = 0x7af
 
kGreek_alphaaccent :: KeySym
kGreek_alphaaccent = 0x7b1
 
kGreek_epsilonaccent :: KeySym
kGreek_epsilonaccent = 0x7b2
 
kGreek_etaaccent :: KeySym
kGreek_etaaccent = 0x7b3
 
kGreek_iotaaccent :: KeySym
kGreek_iotaaccent = 0x7b4
 
kGreek_iotadieresis :: KeySym
kGreek_iotadieresis = 0x7b5
 
kGreek_iotaaccentdieresis :: KeySym
kGreek_iotaaccentdieresis = 0x7b6
 
kGreek_omicronaccent :: KeySym
kGreek_omicronaccent = 0x7b7
 
kGreek_upsilonaccent :: KeySym
kGreek_upsilonaccent = 0x7b8
 
kGreek_upsilondieresis :: KeySym
kGreek_upsilondieresis = 0x7b9
 
kGreek_upsilonaccentdieresis :: KeySym
kGreek_upsilonaccentdieresis = 0x7ba
 
kGreek_omegaaccent :: KeySym
kGreek_omegaaccent = 0x7bb
 
kGreek_ALPHA :: KeySym
kGreek_ALPHA = 0x7c1
 
kGreek_BETA :: KeySym
kGreek_BETA = 0x7c2
 
kGreek_GAMMA :: KeySym
kGreek_GAMMA = 0x7c3
 
kGreek_DELTA :: KeySym
kGreek_DELTA = 0x7c4
 
kGreek_EPSILON :: KeySym
kGreek_EPSILON = 0x7c5
 
kGreek_ZETA :: KeySym
kGreek_ZETA = 0x7c6
 
kGreek_ETA :: KeySym
kGreek_ETA = 0x7c7
 
kGreek_THETA :: KeySym
kGreek_THETA = 0x7c8
 
kGreek_IOTA :: KeySym
kGreek_IOTA = 0x7c9
 
kGreek_KAPPA :: KeySym
kGreek_KAPPA = 0x7ca
 
kGreek_LAMDA :: KeySym
kGreek_LAMDA = 0x7cb
 
kGreek_LAMBDA :: KeySym
kGreek_LAMBDA = 0x7cb
 
kGreek_MU :: KeySym
kGreek_MU = 0x7cc
 
kGreek_NU :: KeySym
kGreek_NU = 0x7cd
 
kGreek_XI :: KeySym
kGreek_XI = 0x7ce
 
kGreek_OMICRON :: KeySym
kGreek_OMICRON = 0x7cf
 
kGreek_PI :: KeySym
kGreek_PI = 0x7d0
 
kGreek_RHO :: KeySym
kGreek_RHO = 0x7d1
 
kGreek_SIGMA :: KeySym
kGreek_SIGMA = 0x7d2
 
kGreek_TAU :: KeySym
kGreek_TAU = 0x7d4
 
kGreek_UPSILON :: KeySym
kGreek_UPSILON = 0x7d5
 
kGreek_PHI :: KeySym
kGreek_PHI = 0x7d6
 
kGreek_CHI :: KeySym
kGreek_CHI = 0x7d7
 
kGreek_PSI :: KeySym
kGreek_PSI = 0x7d8
 
kGreek_OMEGA :: KeySym
kGreek_OMEGA = 0x7d9
 
kGreek_alpha :: KeySym
kGreek_alpha = 0x7e1
 
kGreek_beta :: KeySym
kGreek_beta = 0x7e2
 
kGreek_gamma :: KeySym
kGreek_gamma = 0x7e3
 
kGreek_delta :: KeySym
kGreek_delta = 0x7e4
 
kGreek_epsilon :: KeySym
kGreek_epsilon = 0x7e5
 
kGreek_zeta :: KeySym
kGreek_zeta = 0x7e6
 
kGreek_eta :: KeySym
kGreek_eta = 0x7e7
 
kGreek_theta :: KeySym
kGreek_theta = 0x7e8
 
kGreek_iota :: KeySym
kGreek_iota = 0x7e9
 
kGreek_kappa :: KeySym
kGreek_kappa = 0x7ea
 
kGreek_lamda :: KeySym
kGreek_lamda = 0x7eb
 
kGreek_lambda :: KeySym
kGreek_lambda = 0x7eb
 
kGreek_mu :: KeySym
kGreek_mu = 0x7ec
 
kGreek_nu :: KeySym
kGreek_nu = 0x7ed
 
kGreek_xi :: KeySym
kGreek_xi = 0x7ee
 
kGreek_omicron :: KeySym
kGreek_omicron = 0x7ef
 
kGreek_pi :: KeySym
kGreek_pi = 0x7f0
 
kGreek_rho :: KeySym
kGreek_rho = 0x7f1
 
kGreek_sigma :: KeySym
kGreek_sigma = 0x7f2
 
kGreek_finalsmallsigma :: KeySym
kGreek_finalsmallsigma = 0x7f3
 
kGreek_tau :: KeySym
kGreek_tau = 0x7f4
 
kGreek_upsilon :: KeySym
kGreek_upsilon = 0x7f5
 
kGreek_phi :: KeySym
kGreek_phi = 0x7f6
 
kGreek_chi :: KeySym
kGreek_chi = 0x7f7
 
kGreek_psi :: KeySym
kGreek_psi = 0x7f8
 
kGreek_omega :: KeySym
kGreek_omega = 0x7f9
 
kGreek_switch :: KeySym
kGreek_switch = 0xff7e
 
kleftradical :: KeySym
kleftradical = 0x8a1
 
ktopleftradical :: KeySym
ktopleftradical = 0x8a2
 
khorizconnector :: KeySym
khorizconnector = 0x8a3
 
ktopintegral :: KeySym
ktopintegral = 0x8a4
 
kbotintegral :: KeySym
kbotintegral = 0x8a5
 
kvertconnector :: KeySym
kvertconnector = 0x8a6
 
ktopleftsqbracket :: KeySym
ktopleftsqbracket = 0x8a7
 
kbotleftsqbracket :: KeySym
kbotleftsqbracket = 0x8a8
 
ktoprightsqbracket :: KeySym
ktoprightsqbracket = 0x8a9
 
kbotrightsqbracket :: KeySym
kbotrightsqbracket = 0x8aa
 
ktopleftparens :: KeySym
ktopleftparens = 0x8ab
 
kbotleftparens :: KeySym
kbotleftparens = 0x8ac
 
ktoprightparens :: KeySym
ktoprightparens = 0x8ad
 
kbotrightparens :: KeySym
kbotrightparens = 0x8ae
 
kleftmiddlecurlybrace :: KeySym
kleftmiddlecurlybrace = 0x8af
 
krightmiddlecurlybrace :: KeySym
krightmiddlecurlybrace = 0x8b0
 
ktopleftsummation :: KeySym
ktopleftsummation = 0x8b1
 
kbotleftsummation :: KeySym
kbotleftsummation = 0x8b2
 
ktopvertsummationconnector :: KeySym
ktopvertsummationconnector = 0x8b3
 
kbotvertsummationconnector :: KeySym
kbotvertsummationconnector = 0x8b4
 
ktoprightsummation :: KeySym
ktoprightsummation = 0x8b5
 
kbotrightsummation :: KeySym
kbotrightsummation = 0x8b6
 
krightmiddlesummation :: KeySym
krightmiddlesummation = 0x8b7
 
klessthanequal :: KeySym
klessthanequal = 0x8bc
 
knotequal :: KeySym
knotequal = 0x8bd
 
kgreaterthanequal :: KeySym
kgreaterthanequal = 0x8be
 
kintegral :: KeySym
kintegral = 0x8bf
 
ktherefore :: KeySym
ktherefore = 0x8c0
 
kvariation :: KeySym
kvariation = 0x8c1
 
kinfinity :: KeySym
kinfinity = 0x8c2
 
knabla :: KeySym
knabla = 0x8c5
 
kapproximate :: KeySym
kapproximate = 0x8c8
 
ksimilarequal :: KeySym
ksimilarequal = 0x8c9
 
kifonlyif :: KeySym
kifonlyif = 0x8cd
 
kimplies :: KeySym
kimplies = 0x8ce
 
kidentical :: KeySym
kidentical = 0x8cf
 
kradical :: KeySym
kradical = 0x8d6
 
kincludedin :: KeySym
kincludedin = 0x8da
 
kincludes :: KeySym
kincludes = 0x8db
 
kintersection :: KeySym
kintersection = 0x8dc
 
kunion :: KeySym
kunion = 0x8dd
 
klogicaland :: KeySym
klogicaland = 0x8de
 
klogicalor :: KeySym
klogicalor = 0x8df
 
kpartialderivative :: KeySym
kpartialderivative = 0x8ef
 
kfunction :: KeySym
kfunction = 0x8f6
 
kleftarrow :: KeySym
kleftarrow = 0x8fb
 
kuparrow :: KeySym
kuparrow = 0x8fc
 
krightarrow :: KeySym
krightarrow = 0x8fd
 
kdownarrow :: KeySym
kdownarrow = 0x8fe
 
kblank :: KeySym
kblank = 0x9df
 
ksoliddiamond :: KeySym
ksoliddiamond = 0x9e0
 
kcheckerboard :: KeySym
kcheckerboard = 0x9e1
 
kht :: KeySym
kht = 0x9e2
 
kff :: KeySym
kff = 0x9e3
 
kcr :: KeySym
kcr = 0x9e4
 
klf :: KeySym
klf = 0x9e5
 
knl :: KeySym
knl = 0x9e8
 
kvt :: KeySym
kvt = 0x9e9
 
klowrightcorner :: KeySym
klowrightcorner = 0x9ea
 
kuprightcorner :: KeySym
kuprightcorner = 0x9eb
 
kupleftcorner :: KeySym
kupleftcorner = 0x9ec
 
klowleftcorner :: KeySym
klowleftcorner = 0x9ed
 
kcrossinglines :: KeySym
kcrossinglines = 0x9ee
 
khorizlinescan1 :: KeySym
khorizlinescan1 = 0x9ef
 
khorizlinescan3 :: KeySym
khorizlinescan3 = 0x9f0
 
khorizlinescan5 :: KeySym
khorizlinescan5 = 0x9f1
 
khorizlinescan7 :: KeySym
khorizlinescan7 = 0x9f2
 
khorizlinescan9 :: KeySym
khorizlinescan9 = 0x9f3
 
kleftt :: KeySym
kleftt = 0x9f4
 
krightt :: KeySym
krightt = 0x9f5
 
kbott :: KeySym
kbott = 0x9f6
 
ktopt :: KeySym
ktopt = 0x9f7
 
kvertbar :: KeySym
kvertbar = 0x9f8
 
kemspace :: KeySym
kemspace = 0xaa1
 
kenspace :: KeySym
kenspace = 0xaa2
 
kem3space :: KeySym
kem3space = 0xaa3
 
kem4space :: KeySym
kem4space = 0xaa4
 
kdigitspace :: KeySym
kdigitspace = 0xaa5
 
kpunctspace :: KeySym
kpunctspace = 0xaa6
 
kthinspace :: KeySym
kthinspace = 0xaa7
 
khairspace :: KeySym
khairspace = 0xaa8
 
kemdash :: KeySym
kemdash = 0xaa9
 
kendash :: KeySym
kendash = 0xaaa
 
ksignifblank :: KeySym
ksignifblank = 0xaac
 
kellipsis :: KeySym
kellipsis = 0xaae
 
kdoubbaselinedot :: KeySym
kdoubbaselinedot = 0xaaf
 
konethird :: KeySym
konethird = 0xab0
 
ktwothirds :: KeySym
ktwothirds = 0xab1
 
konefifth :: KeySym
konefifth = 0xab2
 
ktwofifths :: KeySym
ktwofifths = 0xab3
 
kthreefifths :: KeySym
kthreefifths = 0xab4
 
kfourfifths :: KeySym
kfourfifths = 0xab5
 
konesixth :: KeySym
konesixth = 0xab6
 
kfivesixths :: KeySym
kfivesixths = 0xab7
 
kcareof :: KeySym
kcareof = 0xab8
 
kfigdash :: KeySym
kfigdash = 0xabb
 
kleftanglebracket :: KeySym
kleftanglebracket = 0xabc
 
kdecimalpoint :: KeySym
kdecimalpoint = 0xabd
 
krightanglebracket :: KeySym
krightanglebracket = 0xabe
 
kmarker :: KeySym
kmarker = 0xabf
 
koneeighth :: KeySym
koneeighth = 0xac3
 
kthreeeighths :: KeySym
kthreeeighths = 0xac4
 
kfiveeighths :: KeySym
kfiveeighths = 0xac5
 
kseveneighths :: KeySym
kseveneighths = 0xac6
 
ktrademark :: KeySym
ktrademark = 0xac9
 
ksignaturemark :: KeySym
ksignaturemark = 0xaca
 
ktrademarkincircle :: KeySym
ktrademarkincircle = 0xacb
 
kleftopentriangle :: KeySym
kleftopentriangle = 0xacc
 
krightopentriangle :: KeySym
krightopentriangle = 0xacd
 
kemopencircle :: KeySym
kemopencircle = 0xace
 
kemopenrectangle :: KeySym
kemopenrectangle = 0xacf
 
kleftsinglequotemark :: KeySym
kleftsinglequotemark = 0xad0
 
krightsinglequotemark :: KeySym
krightsinglequotemark = 0xad1
 
kleftdoublequotemark :: KeySym
kleftdoublequotemark = 0xad2
 
krightdoublequotemark :: KeySym
krightdoublequotemark = 0xad3
 
kprescription :: KeySym
kprescription = 0xad4
 
kminutes :: KeySym
kminutes = 0xad6
 
kseconds :: KeySym
kseconds = 0xad7
 
klatincross :: KeySym
klatincross = 0xad9
 
khexagram :: KeySym
khexagram = 0xada
 
kfilledrectbullet :: KeySym
kfilledrectbullet = 0xadb
 
kfilledlefttribullet :: KeySym
kfilledlefttribullet = 0xadc
 
kfilledrighttribullet :: KeySym
kfilledrighttribullet = 0xadd
 
kemfilledcircle :: KeySym
kemfilledcircle = 0xade
 
kemfilledrect :: KeySym
kemfilledrect = 0xadf
 
kenopencircbullet :: KeySym
kenopencircbullet = 0xae0
 
kenopensquarebullet :: KeySym
kenopensquarebullet = 0xae1
 
kopenrectbullet :: KeySym
kopenrectbullet = 0xae2
 
kopentribulletup :: KeySym
kopentribulletup = 0xae3
 
kopentribulletdown :: KeySym
kopentribulletdown = 0xae4
 
kopenstar :: KeySym
kopenstar = 0xae5
 
kenfilledcircbullet :: KeySym
kenfilledcircbullet = 0xae6
 
kenfilledsqbullet :: KeySym
kenfilledsqbullet = 0xae7
 
kfilledtribulletup :: KeySym
kfilledtribulletup = 0xae8
 
kfilledtribulletdown :: KeySym
kfilledtribulletdown = 0xae9
 
kleftpointer :: KeySym
kleftpointer = 0xaea
 
krightpointer :: KeySym
krightpointer = 0xaeb
 
kclub :: KeySym
kclub = 0xaec
 
kdiamond :: KeySym
kdiamond = 0xaed
 
kheart :: KeySym
kheart = 0xaee
 
kmaltesecross :: KeySym
kmaltesecross = 0xaf0
 
kdagger :: KeySym
kdagger = 0xaf1
 
kdoubledagger :: KeySym
kdoubledagger = 0xaf2
 
kcheckmark :: KeySym
kcheckmark = 0xaf3
 
kballotcross :: KeySym
kballotcross = 0xaf4
 
kmusicalsharp :: KeySym
kmusicalsharp = 0xaf5
 
kmusicalflat :: KeySym
kmusicalflat = 0xaf6
 
kmalesymbol :: KeySym
kmalesymbol = 0xaf7
 
kfemalesymbol :: KeySym
kfemalesymbol = 0xaf8
 
ktelephone :: KeySym
ktelephone = 0xaf9
 
ktelephonerecorder :: KeySym
ktelephonerecorder = 0xafa
 
kphonographcopyright :: KeySym
kphonographcopyright = 0xafb
 
kcaret :: KeySym
kcaret = 0xafc
 
ksinglelowquotemark :: KeySym
ksinglelowquotemark = 0xafd
 
kdoublelowquotemark :: KeySym
kdoublelowquotemark = 0xafe
 
kcursor :: KeySym
kcursor = 0xaff
 
kleftcaret :: KeySym
kleftcaret = 0xba3
 
krightcaret :: KeySym
krightcaret = 0xba6
 
kdowncaret :: KeySym
kdowncaret = 0xba8
 
kupcaret :: KeySym
kupcaret = 0xba9
 
koverbar :: KeySym
koverbar = 0xbc0
 
kdowntack :: KeySym
kdowntack = 0xbc2
 
kupshoe :: KeySym
kupshoe = 0xbc3
 
kdownstile :: KeySym
kdownstile = 0xbc4
 
kunderbar :: KeySym
kunderbar = 0xbc6
 
kjot :: KeySym
kjot = 0xbca
 
kquad :: KeySym
kquad = 0xbcc
 
kuptack :: KeySym
kuptack = 0xbce
 
kcircle :: KeySym
kcircle = 0xbcf
 
kupstile :: KeySym
kupstile = 0xbd3
 
kdownshoe :: KeySym
kdownshoe = 0xbd6
 
krightshoe :: KeySym
krightshoe = 0xbd8
 
kleftshoe :: KeySym
kleftshoe = 0xbda
 
klefttack :: KeySym
klefttack = 0xbdc
 
krighttack :: KeySym
krighttack = 0xbfc
 
khebrew_doublelowline :: KeySym
khebrew_doublelowline = 0xcdf
 
khebrew_aleph :: KeySym
khebrew_aleph = 0xce0
 
khebrew_bet :: KeySym
khebrew_bet = 0xce1
 
khebrew_beth :: KeySym
khebrew_beth = 0xce1
 
khebrew_gimel :: KeySym
khebrew_gimel = 0xce2
 
khebrew_gimmel :: KeySym
khebrew_gimmel = 0xce2
 
khebrew_dalet :: KeySym
khebrew_dalet = 0xce3
 
khebrew_daleth :: KeySym
khebrew_daleth = 0xce3
 
khebrew_he :: KeySym
khebrew_he = 0xce4
 
khebrew_waw :: KeySym
khebrew_waw = 0xce5
 
khebrew_zain :: KeySym
khebrew_zain = 0xce6
 
khebrew_zayin :: KeySym
khebrew_zayin = 0xce6
 
khebrew_chet :: KeySym
khebrew_chet = 0xce7
 
khebrew_het :: KeySym
khebrew_het = 0xce7
 
khebrew_tet :: KeySym
khebrew_tet = 0xce8
 
khebrew_teth :: KeySym
khebrew_teth = 0xce8
 
khebrew_yod :: KeySym
khebrew_yod = 0xce9
 
khebrew_finalkaph :: KeySym
khebrew_finalkaph = 0xcea
 
khebrew_kaph :: KeySym
khebrew_kaph = 0xceb
 
khebrew_lamed :: KeySym
khebrew_lamed = 0xcec
 
khebrew_finalmem :: KeySym
khebrew_finalmem = 0xced
 
khebrew_mem :: KeySym
khebrew_mem = 0xcee
 
khebrew_finalnun :: KeySym
khebrew_finalnun = 0xcef
 
khebrew_nun :: KeySym
khebrew_nun = 0xcf0
 
khebrew_samech :: KeySym
khebrew_samech = 0xcf1
 
khebrew_samekh :: KeySym
khebrew_samekh = 0xcf1
 
khebrew_ayin :: KeySym
khebrew_ayin = 0xcf2
 
khebrew_finalpe :: KeySym
khebrew_finalpe = 0xcf3
 
khebrew_pe :: KeySym
khebrew_pe = 0xcf4
 
khebrew_finalzade :: KeySym
khebrew_finalzade = 0xcf5
 
khebrew_finalzadi :: KeySym
khebrew_finalzadi = 0xcf5
 
khebrew_zade :: KeySym
khebrew_zade = 0xcf6
 
khebrew_zadi :: KeySym
khebrew_zadi = 0xcf6
 
khebrew_qoph :: KeySym
khebrew_qoph = 0xcf7
 
khebrew_kuf :: KeySym
khebrew_kuf = 0xcf7
 
khebrew_resh :: KeySym
khebrew_resh = 0xcf8
 
khebrew_shin :: KeySym
khebrew_shin = 0xcf9
 
khebrew_taw :: KeySym
khebrew_taw = 0xcfa
 
khebrew_taf :: KeySym
khebrew_taf = 0xcfa
 
kHebrew_switch :: KeySym
kHebrew_switch = 0xff7e
 
kThai_kokai :: KeySym
kThai_kokai = 0xda1
 
kThai_khokhai :: KeySym
kThai_khokhai = 0xda2
 
kThai_khokhuat :: KeySym
kThai_khokhuat = 0xda3
 
kThai_khokhwai :: KeySym
kThai_khokhwai = 0xda4
 
kThai_khokhon :: KeySym
kThai_khokhon = 0xda5
 
kThai_khorakhang :: KeySym
kThai_khorakhang = 0xda6
 
kThai_ngongu :: KeySym
kThai_ngongu = 0xda7
 
kThai_chochan :: KeySym
kThai_chochan = 0xda8
 
kThai_choching :: KeySym
kThai_choching = 0xda9
 
kThai_chochang :: KeySym
kThai_chochang = 0xdaa
 
kThai_soso :: KeySym
kThai_soso = 0xdab
 
kThai_chochoe :: KeySym
kThai_chochoe = 0xdac
 
kThai_yoying :: KeySym
kThai_yoying = 0xdad
 
kThai_dochada :: KeySym
kThai_dochada = 0xdae
 
kThai_topatak :: KeySym
kThai_topatak = 0xdaf
 
kThai_thothan :: KeySym
kThai_thothan = 0xdb0
 
kThai_thonangmontho :: KeySym
kThai_thonangmontho = 0xdb1
 
kThai_thophuthao :: KeySym
kThai_thophuthao = 0xdb2
 
kThai_nonen :: KeySym
kThai_nonen = 0xdb3
 
kThai_dodek :: KeySym
kThai_dodek = 0xdb4
 
kThai_totao :: KeySym
kThai_totao = 0xdb5
 
kThai_thothung :: KeySym
kThai_thothung = 0xdb6
 
kThai_thothahan :: KeySym
kThai_thothahan = 0xdb7
 
kThai_thothong :: KeySym
kThai_thothong = 0xdb8
 
kThai_nonu :: KeySym
kThai_nonu = 0xdb9
 
kThai_bobaimai :: KeySym
kThai_bobaimai = 0xdba
 
kThai_popla :: KeySym
kThai_popla = 0xdbb
 
kThai_phophung :: KeySym
kThai_phophung = 0xdbc
 
kThai_fofa :: KeySym
kThai_fofa = 0xdbd
 
kThai_phophan :: KeySym
kThai_phophan = 0xdbe
 
kThai_fofan :: KeySym
kThai_fofan = 0xdbf
 
kThai_phosamphao :: KeySym
kThai_phosamphao = 0xdc0
 
kThai_moma :: KeySym
kThai_moma = 0xdc1
 
kThai_yoyak :: KeySym
kThai_yoyak = 0xdc2
 
kThai_rorua :: KeySym
kThai_rorua = 0xdc3
 
kThai_ru :: KeySym
kThai_ru = 0xdc4
 
kThai_loling :: KeySym
kThai_loling = 0xdc5
 
kThai_lu :: KeySym
kThai_lu = 0xdc6
 
kThai_wowaen :: KeySym
kThai_wowaen = 0xdc7
 
kThai_sosala :: KeySym
kThai_sosala = 0xdc8
 
kThai_sorusi :: KeySym
kThai_sorusi = 0xdc9
 
kThai_sosua :: KeySym
kThai_sosua = 0xdca
 
kThai_hohip :: KeySym
kThai_hohip = 0xdcb
 
kThai_lochula :: KeySym
kThai_lochula = 0xdcc
 
kThai_oang :: KeySym
kThai_oang = 0xdcd
 
kThai_honokhuk :: KeySym
kThai_honokhuk = 0xdce
 
kThai_paiyannoi :: KeySym
kThai_paiyannoi = 0xdcf
 
kThai_saraa :: KeySym
kThai_saraa = 0xdd0
 
kThai_maihanakat :: KeySym
kThai_maihanakat = 0xdd1
 
kThai_saraaa :: KeySym
kThai_saraaa = 0xdd2
 
kThai_saraam :: KeySym
kThai_saraam = 0xdd3
 
kThai_sarai :: KeySym
kThai_sarai = 0xdd4
 
kThai_saraii :: KeySym
kThai_saraii = 0xdd5
 
kThai_saraue :: KeySym
kThai_saraue = 0xdd6
 
kThai_sarauee :: KeySym
kThai_sarauee = 0xdd7
 
kThai_sarau :: KeySym
kThai_sarau = 0xdd8
 
kThai_sarauu :: KeySym
kThai_sarauu = 0xdd9
 
kThai_phinthu :: KeySym
kThai_phinthu = 0xdda
 
kThai_maihanakat_maitho :: KeySym
kThai_maihanakat_maitho = 0xdde
 
kThai_baht :: KeySym
kThai_baht = 0xddf
 
kThai_sarae :: KeySym
kThai_sarae = 0xde0
 
kThai_saraae :: KeySym
kThai_saraae = 0xde1
 
kThai_sarao :: KeySym
kThai_sarao = 0xde2
 
kThai_saraaimaimuan :: KeySym
kThai_saraaimaimuan = 0xde3
 
kThai_saraaimaimalai :: KeySym
kThai_saraaimaimalai = 0xde4
 
kThai_lakkhangyao :: KeySym
kThai_lakkhangyao = 0xde5
 
kThai_maiyamok :: KeySym
kThai_maiyamok = 0xde6
 
kThai_maitaikhu :: KeySym
kThai_maitaikhu = 0xde7
 
kThai_maiek :: KeySym
kThai_maiek = 0xde8
 
kThai_maitho :: KeySym
kThai_maitho = 0xde9
 
kThai_maitri :: KeySym
kThai_maitri = 0xdea
 
kThai_maichattawa :: KeySym
kThai_maichattawa = 0xdeb
 
kThai_thanthakhat :: KeySym
kThai_thanthakhat = 0xdec
 
kThai_nikhahit :: KeySym
kThai_nikhahit = 0xded
 
kThai_leksun :: KeySym
kThai_leksun = 0xdf0
 
kThai_leknung :: KeySym
kThai_leknung = 0xdf1
 
kThai_leksong :: KeySym
kThai_leksong = 0xdf2
 
kThai_leksam :: KeySym
kThai_leksam = 0xdf3
 
kThai_leksi :: KeySym
kThai_leksi = 0xdf4
 
kThai_lekha :: KeySym
kThai_lekha = 0xdf5
 
kThai_lekhok :: KeySym
kThai_lekhok = 0xdf6
 
kThai_lekchet :: KeySym
kThai_lekchet = 0xdf7
 
kThai_lekpaet :: KeySym
kThai_lekpaet = 0xdf8
 
kThai_lekkao :: KeySym
kThai_lekkao = 0xdf9
 
kHangul :: KeySym
kHangul = 0xff31
 
kHangul_Start :: KeySym
kHangul_Start = 0xff32
 
kHangul_End :: KeySym
kHangul_End = 0xff33
 
kHangul_Hanja :: KeySym
kHangul_Hanja = 0xff34
 
kHangul_Jamo :: KeySym
kHangul_Jamo = 0xff35
 
kHangul_Romaja :: KeySym
kHangul_Romaja = 0xff36
 
kHangul_Codeinput :: KeySym
kHangul_Codeinput = 0xff37
 
kHangul_Jeonja :: KeySym
kHangul_Jeonja = 0xff38
 
kHangul_Banja :: KeySym
kHangul_Banja = 0xff39
 
kHangul_PreHanja :: KeySym
kHangul_PreHanja = 0xff3a
 
kHangul_PostHanja :: KeySym
kHangul_PostHanja = 0xff3b
 
kHangul_SingleCandidate :: KeySym
kHangul_SingleCandidate = 0xff3c
 
kHangul_MultipleCandidate :: KeySym
kHangul_MultipleCandidate = 0xff3d
 
kHangul_PreviousCandidate :: KeySym
kHangul_PreviousCandidate = 0xff3e
 
kHangul_Special :: KeySym
kHangul_Special = 0xff3f
 
kHangul_switch :: KeySym
kHangul_switch = 0xff7e
 
kHangul_Kiyeog :: KeySym
kHangul_Kiyeog = 0xea1
 
kHangul_SsangKiyeog :: KeySym
kHangul_SsangKiyeog = 0xea2
 
kHangul_KiyeogSios :: KeySym
kHangul_KiyeogSios = 0xea3
 
kHangul_Nieun :: KeySym
kHangul_Nieun = 0xea4
 
kHangul_NieunJieuj :: KeySym
kHangul_NieunJieuj = 0xea5
 
kHangul_NieunHieuh :: KeySym
kHangul_NieunHieuh = 0xea6
 
kHangul_Dikeud :: KeySym
kHangul_Dikeud = 0xea7
 
kHangul_SsangDikeud :: KeySym
kHangul_SsangDikeud = 0xea8
 
kHangul_Rieul :: KeySym
kHangul_Rieul = 0xea9
 
kHangul_RieulKiyeog :: KeySym
kHangul_RieulKiyeog = 0xeaa
 
kHangul_RieulMieum :: KeySym
kHangul_RieulMieum = 0xeab
 
kHangul_RieulPieub :: KeySym
kHangul_RieulPieub = 0xeac
 
kHangul_RieulSios :: KeySym
kHangul_RieulSios = 0xead
 
kHangul_RieulTieut :: KeySym
kHangul_RieulTieut = 0xeae
 
kHangul_RieulPhieuf :: KeySym
kHangul_RieulPhieuf = 0xeaf
 
kHangul_RieulHieuh :: KeySym
kHangul_RieulHieuh = 0xeb0
 
kHangul_Mieum :: KeySym
kHangul_Mieum = 0xeb1
 
kHangul_Pieub :: KeySym
kHangul_Pieub = 0xeb2
 
kHangul_SsangPieub :: KeySym
kHangul_SsangPieub = 0xeb3
 
kHangul_PieubSios :: KeySym
kHangul_PieubSios = 0xeb4
 
kHangul_Sios :: KeySym
kHangul_Sios = 0xeb5
 
kHangul_SsangSios :: KeySym
kHangul_SsangSios = 0xeb6
 
kHangul_Ieung :: KeySym
kHangul_Ieung = 0xeb7
 
kHangul_Jieuj :: KeySym
kHangul_Jieuj = 0xeb8
 
kHangul_SsangJieuj :: KeySym
kHangul_SsangJieuj = 0xeb9
 
kHangul_Cieuc :: KeySym
kHangul_Cieuc = 0xeba
 
kHangul_Khieuq :: KeySym
kHangul_Khieuq = 0xebb
 
kHangul_Tieut :: KeySym
kHangul_Tieut = 0xebc
 
kHangul_Phieuf :: KeySym
kHangul_Phieuf = 0xebd
 
kHangul_Hieuh :: KeySym
kHangul_Hieuh = 0xebe
 
kHangul_A :: KeySym
kHangul_A = 0xebf
 
kHangul_AE :: KeySym
kHangul_AE = 0xec0
 
kHangul_YA :: KeySym
kHangul_YA = 0xec1
 
kHangul_YAE :: KeySym
kHangul_YAE = 0xec2
 
kHangul_EO :: KeySym
kHangul_EO = 0xec3
 
kHangul_E :: KeySym
kHangul_E = 0xec4
 
kHangul_YEO :: KeySym
kHangul_YEO = 0xec5
 
kHangul_YE :: KeySym
kHangul_YE = 0xec6
 
kHangul_O :: KeySym
kHangul_O = 0xec7
 
kHangul_WA :: KeySym
kHangul_WA = 0xec8
 
kHangul_WAE :: KeySym
kHangul_WAE = 0xec9
 
kHangul_OE :: KeySym
kHangul_OE = 0xeca
 
kHangul_YO :: KeySym
kHangul_YO = 0xecb
 
kHangul_U :: KeySym
kHangul_U = 0xecc
 
kHangul_WEO :: KeySym
kHangul_WEO = 0xecd
 
kHangul_WE :: KeySym
kHangul_WE = 0xece
 
kHangul_WI :: KeySym
kHangul_WI = 0xecf
 
kHangul_YU :: KeySym
kHangul_YU = 0xed0
 
kHangul_EU :: KeySym
kHangul_EU = 0xed1
 
kHangul_YI :: KeySym
kHangul_YI = 0xed2
 
kHangul_I :: KeySym
kHangul_I = 0xed3
 
kHangul_J_Kiyeog :: KeySym
kHangul_J_Kiyeog = 0xed4
 
kHangul_J_SsangKiyeog :: KeySym
kHangul_J_SsangKiyeog = 0xed5
 
kHangul_J_KiyeogSios :: KeySym
kHangul_J_KiyeogSios = 0xed6
 
kHangul_J_Nieun :: KeySym
kHangul_J_Nieun = 0xed7
 
kHangul_J_NieunJieuj :: KeySym
kHangul_J_NieunJieuj = 0xed8
 
kHangul_J_NieunHieuh :: KeySym
kHangul_J_NieunHieuh = 0xed9
 
kHangul_J_Dikeud :: KeySym
kHangul_J_Dikeud = 0xeda
 
kHangul_J_Rieul :: KeySym
kHangul_J_Rieul = 0xedb
 
kHangul_J_RieulKiyeog :: KeySym
kHangul_J_RieulKiyeog = 0xedc
 
kHangul_J_RieulMieum :: KeySym
kHangul_J_RieulMieum = 0xedd
 
kHangul_J_RieulPieub :: KeySym
kHangul_J_RieulPieub = 0xede
 
kHangul_J_RieulSios :: KeySym
kHangul_J_RieulSios = 0xedf
 
kHangul_J_RieulTieut :: KeySym
kHangul_J_RieulTieut = 0xee0
 
kHangul_J_RieulPhieuf :: KeySym
kHangul_J_RieulPhieuf = 0xee1
 
kHangul_J_RieulHieuh :: KeySym
kHangul_J_RieulHieuh = 0xee2
 
kHangul_J_Mieum :: KeySym
kHangul_J_Mieum = 0xee3
 
kHangul_J_Pieub :: KeySym
kHangul_J_Pieub = 0xee4
 
kHangul_J_PieubSios :: KeySym
kHangul_J_PieubSios = 0xee5
 
kHangul_J_Sios :: KeySym
kHangul_J_Sios = 0xee6
 
kHangul_J_SsangSios :: KeySym
kHangul_J_SsangSios = 0xee7
 
kHangul_J_Ieung :: KeySym
kHangul_J_Ieung = 0xee8
 
kHangul_J_Jieuj :: KeySym
kHangul_J_Jieuj = 0xee9
 
kHangul_J_Cieuc :: KeySym
kHangul_J_Cieuc = 0xeea
 
kHangul_J_Khieuq :: KeySym
kHangul_J_Khieuq = 0xeeb
 
kHangul_J_Tieut :: KeySym
kHangul_J_Tieut = 0xeec
 
kHangul_J_Phieuf :: KeySym
kHangul_J_Phieuf = 0xeed
 
kHangul_J_Hieuh :: KeySym
kHangul_J_Hieuh = 0xeee
 
kHangul_RieulYeorinHieuh :: KeySym
kHangul_RieulYeorinHieuh = 0xeef
 
kHangul_SunkyeongeumMieum :: KeySym
kHangul_SunkyeongeumMieum = 0xef0
 
kHangul_SunkyeongeumPieub :: KeySym
kHangul_SunkyeongeumPieub = 0xef1
 
kHangul_PanSios :: KeySym
kHangul_PanSios = 0xef2
 
kHangul_KkogjiDalrinIeung :: KeySym
kHangul_KkogjiDalrinIeung = 0xef3
 
kHangul_SunkyeongeumPhieuf :: KeySym
kHangul_SunkyeongeumPhieuf = 0xef4
 
kHangul_YeorinHieuh :: KeySym
kHangul_YeorinHieuh = 0xef5
 
kHangul_AraeA :: KeySym
kHangul_AraeA = 0xef6
 
kHangul_AraeAE :: KeySym
kHangul_AraeAE = 0xef7
 
kHangul_J_PanSios :: KeySym
kHangul_J_PanSios = 0xef8
 
kHangul_J_KkogjiDalrinIeung :: KeySym
kHangul_J_KkogjiDalrinIeung = 0xef9
 
kHangul_J_YeorinHieuh :: KeySym
kHangul_J_YeorinHieuh = 0xefa
 
kKorean_Won :: KeySym
kKorean_Won = 0xeff
 
kArmenian_ligature_ew :: KeySym
kArmenian_ligature_ew = 0x1000587
 
kArmenian_full_stop :: KeySym
kArmenian_full_stop = 0x1000589
 
kArmenian_verjaket :: KeySym
kArmenian_verjaket = 0x1000589
 
kArmenian_separation_mark :: KeySym
kArmenian_separation_mark = 0x100055d
 
kArmenian_but :: KeySym
kArmenian_but = 0x100055d
 
kArmenian_hyphen :: KeySym
kArmenian_hyphen = 0x100058a
 
kArmenian_yentamna :: KeySym
kArmenian_yentamna = 0x100058a
 
kArmenian_exclam :: KeySym
kArmenian_exclam = 0x100055c
 
kArmenian_amanak :: KeySym
kArmenian_amanak = 0x100055c
 
kArmenian_accent :: KeySym
kArmenian_accent = 0x100055b
 
kArmenian_shesht :: KeySym
kArmenian_shesht = 0x100055b
 
kArmenian_question :: KeySym
kArmenian_question = 0x100055e
 
kArmenian_paruyk :: KeySym
kArmenian_paruyk = 0x100055e
 
kArmenian_AYB :: KeySym
kArmenian_AYB = 0x1000531
 
kArmenian_ayb :: KeySym
kArmenian_ayb = 0x1000561
 
kArmenian_BEN :: KeySym
kArmenian_BEN = 0x1000532
 
kArmenian_ben :: KeySym
kArmenian_ben = 0x1000562
 
kArmenian_GIM :: KeySym
kArmenian_GIM = 0x1000533
 
kArmenian_gim :: KeySym
kArmenian_gim = 0x1000563
 
kArmenian_DA :: KeySym
kArmenian_DA = 0x1000534
 
kArmenian_da :: KeySym
kArmenian_da = 0x1000564
 
kArmenian_YECH :: KeySym
kArmenian_YECH = 0x1000535
 
kArmenian_yech :: KeySym
kArmenian_yech = 0x1000565
 
kArmenian_ZA :: KeySym
kArmenian_ZA = 0x1000536
 
kArmenian_za :: KeySym
kArmenian_za = 0x1000566
 
kArmenian_E :: KeySym
kArmenian_E = 0x1000537
 
kArmenian_e :: KeySym
kArmenian_e = 0x1000567
 
kArmenian_AT :: KeySym
kArmenian_AT = 0x1000538
 
kArmenian_at :: KeySym
kArmenian_at = 0x1000568
 
kArmenian_TO :: KeySym
kArmenian_TO = 0x1000539
 
kArmenian_to :: KeySym
kArmenian_to = 0x1000569
 
kArmenian_ZHE :: KeySym
kArmenian_ZHE = 0x100053a
 
kArmenian_zhe :: KeySym
kArmenian_zhe = 0x100056a
 
kArmenian_INI :: KeySym
kArmenian_INI = 0x100053b
 
kArmenian_ini :: KeySym
kArmenian_ini = 0x100056b
 
kArmenian_LYUN :: KeySym
kArmenian_LYUN = 0x100053c
 
kArmenian_lyun :: KeySym
kArmenian_lyun = 0x100056c
 
kArmenian_KHE :: KeySym
kArmenian_KHE = 0x100053d
 
kArmenian_khe :: KeySym
kArmenian_khe = 0x100056d
 
kArmenian_TSA :: KeySym
kArmenian_TSA = 0x100053e
 
kArmenian_tsa :: KeySym
kArmenian_tsa = 0x100056e
 
kArmenian_KEN :: KeySym
kArmenian_KEN = 0x100053f
 
kArmenian_ken :: KeySym
kArmenian_ken = 0x100056f
 
kArmenian_HO :: KeySym
kArmenian_HO = 0x1000540
 
kArmenian_ho :: KeySym
kArmenian_ho = 0x1000570
 
kArmenian_DZA :: KeySym
kArmenian_DZA = 0x1000541
 
kArmenian_dza :: KeySym
kArmenian_dza = 0x1000571
 
kArmenian_GHAT :: KeySym
kArmenian_GHAT = 0x1000542
 
kArmenian_ghat :: KeySym
kArmenian_ghat = 0x1000572
 
kArmenian_TCHE :: KeySym
kArmenian_TCHE = 0x1000543
 
kArmenian_tche :: KeySym
kArmenian_tche = 0x1000573
 
kArmenian_MEN :: KeySym
kArmenian_MEN = 0x1000544
 
kArmenian_men :: KeySym
kArmenian_men = 0x1000574
 
kArmenian_HI :: KeySym
kArmenian_HI = 0x1000545
 
kArmenian_hi :: KeySym
kArmenian_hi = 0x1000575
 
kArmenian_NU :: KeySym
kArmenian_NU = 0x1000546
 
kArmenian_nu :: KeySym
kArmenian_nu = 0x1000576
 
kArmenian_SHA :: KeySym
kArmenian_SHA = 0x1000547
 
kArmenian_sha :: KeySym
kArmenian_sha = 0x1000577
 
kArmenian_VO :: KeySym
kArmenian_VO = 0x1000548
 
kArmenian_vo :: KeySym
kArmenian_vo = 0x1000578
 
kArmenian_CHA :: KeySym
kArmenian_CHA = 0x1000549
 
kArmenian_cha :: KeySym
kArmenian_cha = 0x1000579
 
kArmenian_PE :: KeySym
kArmenian_PE = 0x100054a
 
kArmenian_pe :: KeySym
kArmenian_pe = 0x100057a
 
kArmenian_JE :: KeySym
kArmenian_JE = 0x100054b
 
kArmenian_je :: KeySym
kArmenian_je = 0x100057b
 
kArmenian_RA :: KeySym
kArmenian_RA = 0x100054c
 
kArmenian_ra :: KeySym
kArmenian_ra = 0x100057c
 
kArmenian_SE :: KeySym
kArmenian_SE = 0x100054d
 
kArmenian_se :: KeySym
kArmenian_se = 0x100057d
 
kArmenian_VEV :: KeySym
kArmenian_VEV = 0x100054e
 
kArmenian_vev :: KeySym
kArmenian_vev = 0x100057e
 
kArmenian_TYUN :: KeySym
kArmenian_TYUN = 0x100054f
 
kArmenian_tyun :: KeySym
kArmenian_tyun = 0x100057f
 
kArmenian_RE :: KeySym
kArmenian_RE = 0x1000550
 
kArmenian_re :: KeySym
kArmenian_re = 0x1000580
 
kArmenian_TSO :: KeySym
kArmenian_TSO = 0x1000551
 
kArmenian_tso :: KeySym
kArmenian_tso = 0x1000581
 
kArmenian_VYUN :: KeySym
kArmenian_VYUN = 0x1000552
 
kArmenian_vyun :: KeySym
kArmenian_vyun = 0x1000582
 
kArmenian_PYUR :: KeySym
kArmenian_PYUR = 0x1000553
 
kArmenian_pyur :: KeySym
kArmenian_pyur = 0x1000583
 
kArmenian_KE :: KeySym
kArmenian_KE = 0x1000554
 
kArmenian_ke :: KeySym
kArmenian_ke = 0x1000584
 
kArmenian_O :: KeySym
kArmenian_O = 0x1000555
 
kArmenian_o :: KeySym
kArmenian_o = 0x1000585
 
kArmenian_FE :: KeySym
kArmenian_FE = 0x1000556
 
kArmenian_fe :: KeySym
kArmenian_fe = 0x1000586
 
kArmenian_apostrophe :: KeySym
kArmenian_apostrophe = 0x100055a
 
kGeorgian_an :: KeySym
kGeorgian_an = 0x10010d0
 
kGeorgian_ban :: KeySym
kGeorgian_ban = 0x10010d1
 
kGeorgian_gan :: KeySym
kGeorgian_gan = 0x10010d2
 
kGeorgian_don :: KeySym
kGeorgian_don = 0x10010d3
 
kGeorgian_en :: KeySym
kGeorgian_en = 0x10010d4
 
kGeorgian_vin :: KeySym
kGeorgian_vin = 0x10010d5
 
kGeorgian_zen :: KeySym
kGeorgian_zen = 0x10010d6
 
kGeorgian_tan :: KeySym
kGeorgian_tan = 0x10010d7
 
kGeorgian_in :: KeySym
kGeorgian_in = 0x10010d8
 
kGeorgian_kan :: KeySym
kGeorgian_kan = 0x10010d9
 
kGeorgian_las :: KeySym
kGeorgian_las = 0x10010da
 
kGeorgian_man :: KeySym
kGeorgian_man = 0x10010db
 
kGeorgian_nar :: KeySym
kGeorgian_nar = 0x10010dc
 
kGeorgian_on :: KeySym
kGeorgian_on = 0x10010dd
 
kGeorgian_par :: KeySym
kGeorgian_par = 0x10010de
 
kGeorgian_zhar :: KeySym
kGeorgian_zhar = 0x10010df
 
kGeorgian_rae :: KeySym
kGeorgian_rae = 0x10010e0
 
kGeorgian_san :: KeySym
kGeorgian_san = 0x10010e1
 
kGeorgian_tar :: KeySym
kGeorgian_tar = 0x10010e2
 
kGeorgian_un :: KeySym
kGeorgian_un = 0x10010e3
 
kGeorgian_phar :: KeySym
kGeorgian_phar = 0x10010e4
 
kGeorgian_khar :: KeySym
kGeorgian_khar = 0x10010e5
 
kGeorgian_ghan :: KeySym
kGeorgian_ghan = 0x10010e6
 
kGeorgian_qar :: KeySym
kGeorgian_qar = 0x10010e7
 
kGeorgian_shin :: KeySym
kGeorgian_shin = 0x10010e8
 
kGeorgian_chin :: KeySym
kGeorgian_chin = 0x10010e9
 
kGeorgian_can :: KeySym
kGeorgian_can = 0x10010ea
 
kGeorgian_jil :: KeySym
kGeorgian_jil = 0x10010eb
 
kGeorgian_cil :: KeySym
kGeorgian_cil = 0x10010ec
 
kGeorgian_char :: KeySym
kGeorgian_char = 0x10010ed
 
kGeorgian_xan :: KeySym
kGeorgian_xan = 0x10010ee
 
kGeorgian_jhan :: KeySym
kGeorgian_jhan = 0x10010ef
 
kGeorgian_hae :: KeySym
kGeorgian_hae = 0x10010f0
 
kGeorgian_he :: KeySym
kGeorgian_he = 0x10010f1
 
kGeorgian_hie :: KeySym
kGeorgian_hie = 0x10010f2
 
kGeorgian_we :: KeySym
kGeorgian_we = 0x10010f3
 
kGeorgian_har :: KeySym
kGeorgian_har = 0x10010f4
 
kGeorgian_hoe :: KeySym
kGeorgian_hoe = 0x10010f5
 
kGeorgian_fi :: KeySym
kGeorgian_fi = 0x10010f6
 
kXabovedot :: KeySym
kXabovedot = 0x1001e8a
 
kIbreve :: KeySym
kIbreve = 0x100012c
 
kZstroke :: KeySym
kZstroke = 0x10001b5
 
kGcaron :: KeySym
kGcaron = 0x10001e6
 
kOcaron :: KeySym
kOcaron = 0x10001d1
 
kObarred :: KeySym
kObarred = 0x100019f
 
kxabovedot :: KeySym
kxabovedot = 0x1001e8b
 
kibreve :: KeySym
kibreve = 0x100012d
 
kzstroke :: KeySym
kzstroke = 0x10001b6
 
kgcaron :: KeySym
kgcaron = 0x10001e7
 
kocaron :: KeySym
kocaron = 0x10001d2
 
kobarred :: KeySym
kobarred = 0x1000275
 
kSCHWA :: KeySym
kSCHWA = 0x100018f
 
kschwa :: KeySym
kschwa = 0x1000259
 
kLbelowdot :: KeySym
kLbelowdot = 0x1001e36
 
klbelowdot :: KeySym
klbelowdot = 0x1001e37
 
kAbelowdot :: KeySym
kAbelowdot = 0x1001ea0
 
kabelowdot :: KeySym
kabelowdot = 0x1001ea1
 
kAhook :: KeySym
kAhook = 0x1001ea2
 
kahook :: KeySym
kahook = 0x1001ea3
 
kAcircumflexacute :: KeySym
kAcircumflexacute = 0x1001ea4
 
kacircumflexacute :: KeySym
kacircumflexacute = 0x1001ea5
 
kAcircumflexgrave :: KeySym
kAcircumflexgrave = 0x1001ea6
 
kacircumflexgrave :: KeySym
kacircumflexgrave = 0x1001ea7
 
kAcircumflexhook :: KeySym
kAcircumflexhook = 0x1001ea8
 
kacircumflexhook :: KeySym
kacircumflexhook = 0x1001ea9
 
kAcircumflextilde :: KeySym
kAcircumflextilde = 0x1001eaa
 
kacircumflextilde :: KeySym
kacircumflextilde = 0x1001eab
 
kAcircumflexbelowdot :: KeySym
kAcircumflexbelowdot = 0x1001eac
 
kacircumflexbelowdot :: KeySym
kacircumflexbelowdot = 0x1001ead
 
kAbreveacute :: KeySym
kAbreveacute = 0x1001eae
 
kabreveacute :: KeySym
kabreveacute = 0x1001eaf
 
kAbrevegrave :: KeySym
kAbrevegrave = 0x1001eb0
 
kabrevegrave :: KeySym
kabrevegrave = 0x1001eb1
 
kAbrevehook :: KeySym
kAbrevehook = 0x1001eb2
 
kabrevehook :: KeySym
kabrevehook = 0x1001eb3
 
kAbrevetilde :: KeySym
kAbrevetilde = 0x1001eb4
 
kabrevetilde :: KeySym
kabrevetilde = 0x1001eb5
 
kAbrevebelowdot :: KeySym
kAbrevebelowdot = 0x1001eb6
 
kabrevebelowdot :: KeySym
kabrevebelowdot = 0x1001eb7
 
kEbelowdot :: KeySym
kEbelowdot = 0x1001eb8
 
kebelowdot :: KeySym
kebelowdot = 0x1001eb9
 
kEhook :: KeySym
kEhook = 0x1001eba
 
kehook :: KeySym
kehook = 0x1001ebb
 
kEtilde :: KeySym
kEtilde = 0x1001ebc
 
ketilde :: KeySym
ketilde = 0x1001ebd
 
kEcircumflexacute :: KeySym
kEcircumflexacute = 0x1001ebe
 
kecircumflexacute :: KeySym
kecircumflexacute = 0x1001ebf
 
kEcircumflexgrave :: KeySym
kEcircumflexgrave = 0x1001ec0
 
kecircumflexgrave :: KeySym
kecircumflexgrave = 0x1001ec1
 
kEcircumflexhook :: KeySym
kEcircumflexhook = 0x1001ec2
 
kecircumflexhook :: KeySym
kecircumflexhook = 0x1001ec3
 
kEcircumflextilde :: KeySym
kEcircumflextilde = 0x1001ec4
 
kecircumflextilde :: KeySym
kecircumflextilde = 0x1001ec5
 
kEcircumflexbelowdot :: KeySym
kEcircumflexbelowdot = 0x1001ec6
 
kecircumflexbelowdot :: KeySym
kecircumflexbelowdot = 0x1001ec7
 
kIhook :: KeySym
kIhook = 0x1001ec8
 
kihook :: KeySym
kihook = 0x1001ec9
 
kIbelowdot :: KeySym
kIbelowdot = 0x1001eca
 
kibelowdot :: KeySym
kibelowdot = 0x1001ecb
 
kObelowdot :: KeySym
kObelowdot = 0x1001ecc
 
kobelowdot :: KeySym
kobelowdot = 0x1001ecd
 
kOhook :: KeySym
kOhook = 0x1001ece
 
kohook :: KeySym
kohook = 0x1001ecf
 
kOcircumflexacute :: KeySym
kOcircumflexacute = 0x1001ed0
 
kocircumflexacute :: KeySym
kocircumflexacute = 0x1001ed1
 
kOcircumflexgrave :: KeySym
kOcircumflexgrave = 0x1001ed2
 
kocircumflexgrave :: KeySym
kocircumflexgrave = 0x1001ed3
 
kOcircumflexhook :: KeySym
kOcircumflexhook = 0x1001ed4
 
kocircumflexhook :: KeySym
kocircumflexhook = 0x1001ed5
 
kOcircumflextilde :: KeySym
kOcircumflextilde = 0x1001ed6
 
kocircumflextilde :: KeySym
kocircumflextilde = 0x1001ed7
 
kOcircumflexbelowdot :: KeySym
kOcircumflexbelowdot = 0x1001ed8
 
kocircumflexbelowdot :: KeySym
kocircumflexbelowdot = 0x1001ed9
 
kOhornacute :: KeySym
kOhornacute = 0x1001eda
 
kohornacute :: KeySym
kohornacute = 0x1001edb
 
kOhorngrave :: KeySym
kOhorngrave = 0x1001edc
 
kohorngrave :: KeySym
kohorngrave = 0x1001edd
 
kOhornhook :: KeySym
kOhornhook = 0x1001ede
 
kohornhook :: KeySym
kohornhook = 0x1001edf
 
kOhorntilde :: KeySym
kOhorntilde = 0x1001ee0
 
kohorntilde :: KeySym
kohorntilde = 0x1001ee1
 
kOhornbelowdot :: KeySym
kOhornbelowdot = 0x1001ee2
 
kohornbelowdot :: KeySym
kohornbelowdot = 0x1001ee3
 
kUbelowdot :: KeySym
kUbelowdot = 0x1001ee4
 
kubelowdot :: KeySym
kubelowdot = 0x1001ee5
 
kUhook :: KeySym
kUhook = 0x1001ee6
 
kuhook :: KeySym
kuhook = 0x1001ee7
 
kUhornacute :: KeySym
kUhornacute = 0x1001ee8
 
kuhornacute :: KeySym
kuhornacute = 0x1001ee9
 
kUhorngrave :: KeySym
kUhorngrave = 0x1001eea
 
kuhorngrave :: KeySym
kuhorngrave = 0x1001eeb
 
kUhornhook :: KeySym
kUhornhook = 0x1001eec
 
kuhornhook :: KeySym
kuhornhook = 0x1001eed
 
kUhorntilde :: KeySym
kUhorntilde = 0x1001eee
 
kuhorntilde :: KeySym
kuhorntilde = 0x1001eef
 
kUhornbelowdot :: KeySym
kUhornbelowdot = 0x1001ef0
 
kuhornbelowdot :: KeySym
kuhornbelowdot = 0x1001ef1
 
kYbelowdot :: KeySym
kYbelowdot = 0x1001ef4
 
kybelowdot :: KeySym
kybelowdot = 0x1001ef5
 
kYhook :: KeySym
kYhook = 0x1001ef6
 
kyhook :: KeySym
kyhook = 0x1001ef7
 
kYtilde :: KeySym
kYtilde = 0x1001ef8
 
kytilde :: KeySym
kytilde = 0x1001ef9
 
kOhorn :: KeySym
kOhorn = 0x10001a0
 
kohorn :: KeySym
kohorn = 0x10001a1
 
kUhorn :: KeySym
kUhorn = 0x10001af
 
kuhorn :: KeySym
kuhorn = 0x10001b0
 
kEcuSign :: KeySym
kEcuSign = 0x10020a0
 
kColonSign :: KeySym
kColonSign = 0x10020a1
 
kCruzeiroSign :: KeySym
kCruzeiroSign = 0x10020a2
 
kFFrancSign :: KeySym
kFFrancSign = 0x10020a3
 
kLiraSign :: KeySym
kLiraSign = 0x10020a4
 
kMillSign :: KeySym
kMillSign = 0x10020a5
 
kNairaSign :: KeySym
kNairaSign = 0x10020a6
 
kPesetaSign :: KeySym
kPesetaSign = 0x10020a7
 
kRupeeSign :: KeySym
kRupeeSign = 0x10020a8
 
kWonSign :: KeySym
kWonSign = 0x10020a9
 
kNewSheqelSign :: KeySym
kNewSheqelSign = 0x10020aa
 
kDongSign :: KeySym
kDongSign = 0x10020ab
 
kEuroSign :: KeySym
kEuroSign = 0x20ac
 
kzerosuperior :: KeySym
kzerosuperior = 0x1002070
 
kfoursuperior :: KeySym
kfoursuperior = 0x1002074
 
kfivesuperior :: KeySym
kfivesuperior = 0x1002075
 
ksixsuperior :: KeySym
ksixsuperior = 0x1002076
 
ksevensuperior :: KeySym
ksevensuperior = 0x1002077
 
keightsuperior :: KeySym
keightsuperior = 0x1002078
 
kninesuperior :: KeySym
kninesuperior = 0x1002079
 
kzerosubscript :: KeySym
kzerosubscript = 0x1002080
 
konesubscript :: KeySym
konesubscript = 0x1002081
 
ktwosubscript :: KeySym
ktwosubscript = 0x1002082
 
kthreesubscript :: KeySym
kthreesubscript = 0x1002083
 
kfoursubscript :: KeySym
kfoursubscript = 0x1002084
 
kfivesubscript :: KeySym
kfivesubscript = 0x1002085
 
ksixsubscript :: KeySym
ksixsubscript = 0x1002086
 
ksevensubscript :: KeySym
ksevensubscript = 0x1002087
 
keightsubscript :: KeySym
keightsubscript = 0x1002088
 
kninesubscript :: KeySym
kninesubscript = 0x1002089
 
kpartdifferential :: KeySym
kpartdifferential = 0x1002202
 
kemptyset :: KeySym
kemptyset = 0x1002205
 
kelementof :: KeySym
kelementof = 0x1002208
 
knotelementof :: KeySym
knotelementof = 0x1002209
 
kcontainsas :: KeySym
kcontainsas = 0x100220b
 
ksquareroot :: KeySym
ksquareroot = 0x100221a
 
kcuberoot :: KeySym
kcuberoot = 0x100221b
 
kfourthroot :: KeySym
kfourthroot = 0x100221c
 
kdintegral :: KeySym
kdintegral = 0x100222c
 
ktintegral :: KeySym
ktintegral = 0x100222d
 
kbecause :: KeySym
kbecause = 0x1002235
 
kapproxeq :: KeySym
kapproxeq = 0x1002248
 
knotapproxeq :: KeySym
knotapproxeq = 0x1002247
 
knotidentical :: KeySym
knotidentical = 0x1002262
 
kstricteq :: KeySym
kstricteq = 0x1002263
 
kbraille_dot_1 :: KeySym
kbraille_dot_1 = 0xfff1
 
kbraille_dot_2 :: KeySym
kbraille_dot_2 = 0xfff2
 
kbraille_dot_3 :: KeySym
kbraille_dot_3 = 0xfff3
 
kbraille_dot_4 :: KeySym
kbraille_dot_4 = 0xfff4
 
kbraille_dot_5 :: KeySym
kbraille_dot_5 = 0xfff5
 
kbraille_dot_6 :: KeySym
kbraille_dot_6 = 0xfff6
 
kbraille_dot_7 :: KeySym
kbraille_dot_7 = 0xfff7
 
kbraille_dot_8 :: KeySym
kbraille_dot_8 = 0xfff8
 
kbraille_dot_9 :: KeySym
kbraille_dot_9 = 0xfff9
 
kbraille_dot_10 :: KeySym
kbraille_dot_10 = 0xfffa
 
kbraille_blank :: KeySym
kbraille_blank = 0x1002800
 
kbraille_dots_1 :: KeySym
kbraille_dots_1 = 0x1002801
 
kbraille_dots_2 :: KeySym
kbraille_dots_2 = 0x1002802
 
kbraille_dots_12 :: KeySym
kbraille_dots_12 = 0x1002803
 
kbraille_dots_3 :: KeySym
kbraille_dots_3 = 0x1002804
 
kbraille_dots_13 :: KeySym
kbraille_dots_13 = 0x1002805
 
kbraille_dots_23 :: KeySym
kbraille_dots_23 = 0x1002806
 
kbraille_dots_123 :: KeySym
kbraille_dots_123 = 0x1002807
 
kbraille_dots_4 :: KeySym
kbraille_dots_4 = 0x1002808
 
kbraille_dots_14 :: KeySym
kbraille_dots_14 = 0x1002809
 
kbraille_dots_24 :: KeySym
kbraille_dots_24 = 0x100280a
 
kbraille_dots_124 :: KeySym
kbraille_dots_124 = 0x100280b
 
kbraille_dots_34 :: KeySym
kbraille_dots_34 = 0x100280c
 
kbraille_dots_134 :: KeySym
kbraille_dots_134 = 0x100280d
 
kbraille_dots_234 :: KeySym
kbraille_dots_234 = 0x100280e
 
kbraille_dots_1234 :: KeySym
kbraille_dots_1234 = 0x100280f
 
kbraille_dots_5 :: KeySym
kbraille_dots_5 = 0x1002810
 
kbraille_dots_15 :: KeySym
kbraille_dots_15 = 0x1002811
 
kbraille_dots_25 :: KeySym
kbraille_dots_25 = 0x1002812
 
kbraille_dots_125 :: KeySym
kbraille_dots_125 = 0x1002813
 
kbraille_dots_35 :: KeySym
kbraille_dots_35 = 0x1002814
 
kbraille_dots_135 :: KeySym
kbraille_dots_135 = 0x1002815
 
kbraille_dots_235 :: KeySym
kbraille_dots_235 = 0x1002816
 
kbraille_dots_1235 :: KeySym
kbraille_dots_1235 = 0x1002817
 
kbraille_dots_45 :: KeySym
kbraille_dots_45 = 0x1002818
 
kbraille_dots_145 :: KeySym
kbraille_dots_145 = 0x1002819
 
kbraille_dots_245 :: KeySym
kbraille_dots_245 = 0x100281a
 
kbraille_dots_1245 :: KeySym
kbraille_dots_1245 = 0x100281b
 
kbraille_dots_345 :: KeySym
kbraille_dots_345 = 0x100281c
 
kbraille_dots_1345 :: KeySym
kbraille_dots_1345 = 0x100281d
 
kbraille_dots_2345 :: KeySym
kbraille_dots_2345 = 0x100281e
 
kbraille_dots_12345 :: KeySym
kbraille_dots_12345 = 0x100281f
 
kbraille_dots_6 :: KeySym
kbraille_dots_6 = 0x1002820
 
kbraille_dots_16 :: KeySym
kbraille_dots_16 = 0x1002821
 
kbraille_dots_26 :: KeySym
kbraille_dots_26 = 0x1002822
 
kbraille_dots_126 :: KeySym
kbraille_dots_126 = 0x1002823
 
kbraille_dots_36 :: KeySym
kbraille_dots_36 = 0x1002824
 
kbraille_dots_136 :: KeySym
kbraille_dots_136 = 0x1002825
 
kbraille_dots_236 :: KeySym
kbraille_dots_236 = 0x1002826
 
kbraille_dots_1236 :: KeySym
kbraille_dots_1236 = 0x1002827
 
kbraille_dots_46 :: KeySym
kbraille_dots_46 = 0x1002828
 
kbraille_dots_146 :: KeySym
kbraille_dots_146 = 0x1002829
 
kbraille_dots_246 :: KeySym
kbraille_dots_246 = 0x100282a
 
kbraille_dots_1246 :: KeySym
kbraille_dots_1246 = 0x100282b
 
kbraille_dots_346 :: KeySym
kbraille_dots_346 = 0x100282c
 
kbraille_dots_1346 :: KeySym
kbraille_dots_1346 = 0x100282d
 
kbraille_dots_2346 :: KeySym
kbraille_dots_2346 = 0x100282e
 
kbraille_dots_12346 :: KeySym
kbraille_dots_12346 = 0x100282f
 
kbraille_dots_56 :: KeySym
kbraille_dots_56 = 0x1002830
 
kbraille_dots_156 :: KeySym
kbraille_dots_156 = 0x1002831
 
kbraille_dots_256 :: KeySym
kbraille_dots_256 = 0x1002832
 
kbraille_dots_1256 :: KeySym
kbraille_dots_1256 = 0x1002833
 
kbraille_dots_356 :: KeySym
kbraille_dots_356 = 0x1002834
 
kbraille_dots_1356 :: KeySym
kbraille_dots_1356 = 0x1002835
 
kbraille_dots_2356 :: KeySym
kbraille_dots_2356 = 0x1002836
 
kbraille_dots_12356 :: KeySym
kbraille_dots_12356 = 0x1002837
 
kbraille_dots_456 :: KeySym
kbraille_dots_456 = 0x1002838
 
kbraille_dots_1456 :: KeySym
kbraille_dots_1456 = 0x1002839
 
kbraille_dots_2456 :: KeySym
kbraille_dots_2456 = 0x100283a
 
kbraille_dots_12456 :: KeySym
kbraille_dots_12456 = 0x100283b
 
kbraille_dots_3456 :: KeySym
kbraille_dots_3456 = 0x100283c
 
kbraille_dots_13456 :: KeySym
kbraille_dots_13456 = 0x100283d
 
kbraille_dots_23456 :: KeySym
kbraille_dots_23456 = 0x100283e
 
kbraille_dots_123456 :: KeySym
kbraille_dots_123456 = 0x100283f
 
kbraille_dots_7 :: KeySym
kbraille_dots_7 = 0x1002840
 
kbraille_dots_17 :: KeySym
kbraille_dots_17 = 0x1002841
 
kbraille_dots_27 :: KeySym
kbraille_dots_27 = 0x1002842
 
kbraille_dots_127 :: KeySym
kbraille_dots_127 = 0x1002843
 
kbraille_dots_37 :: KeySym
kbraille_dots_37 = 0x1002844
 
kbraille_dots_137 :: KeySym
kbraille_dots_137 = 0x1002845
 
kbraille_dots_237 :: KeySym
kbraille_dots_237 = 0x1002846
 
kbraille_dots_1237 :: KeySym
kbraille_dots_1237 = 0x1002847
 
kbraille_dots_47 :: KeySym
kbraille_dots_47 = 0x1002848
 
kbraille_dots_147 :: KeySym
kbraille_dots_147 = 0x1002849
 
kbraille_dots_247 :: KeySym
kbraille_dots_247 = 0x100284a
 
kbraille_dots_1247 :: KeySym
kbraille_dots_1247 = 0x100284b
 
kbraille_dots_347 :: KeySym
kbraille_dots_347 = 0x100284c
 
kbraille_dots_1347 :: KeySym
kbraille_dots_1347 = 0x100284d
 
kbraille_dots_2347 :: KeySym
kbraille_dots_2347 = 0x100284e
 
kbraille_dots_12347 :: KeySym
kbraille_dots_12347 = 0x100284f
 
kbraille_dots_57 :: KeySym
kbraille_dots_57 = 0x1002850
 
kbraille_dots_157 :: KeySym
kbraille_dots_157 = 0x1002851
 
kbraille_dots_257 :: KeySym
kbraille_dots_257 = 0x1002852
 
kbraille_dots_1257 :: KeySym
kbraille_dots_1257 = 0x1002853
 
kbraille_dots_357 :: KeySym
kbraille_dots_357 = 0x1002854
 
kbraille_dots_1357 :: KeySym
kbraille_dots_1357 = 0x1002855
 
kbraille_dots_2357 :: KeySym
kbraille_dots_2357 = 0x1002856
 
kbraille_dots_12357 :: KeySym
kbraille_dots_12357 = 0x1002857
 
kbraille_dots_457 :: KeySym
kbraille_dots_457 = 0x1002858
 
kbraille_dots_1457 :: KeySym
kbraille_dots_1457 = 0x1002859
 
kbraille_dots_2457 :: KeySym
kbraille_dots_2457 = 0x100285a
 
kbraille_dots_12457 :: KeySym
kbraille_dots_12457 = 0x100285b
 
kbraille_dots_3457 :: KeySym
kbraille_dots_3457 = 0x100285c
 
kbraille_dots_13457 :: KeySym
kbraille_dots_13457 = 0x100285d
 
kbraille_dots_23457 :: KeySym
kbraille_dots_23457 = 0x100285e
 
kbraille_dots_123457 :: KeySym
kbraille_dots_123457 = 0x100285f
 
kbraille_dots_67 :: KeySym
kbraille_dots_67 = 0x1002860
 
kbraille_dots_167 :: KeySym
kbraille_dots_167 = 0x1002861
 
kbraille_dots_267 :: KeySym
kbraille_dots_267 = 0x1002862
 
kbraille_dots_1267 :: KeySym
kbraille_dots_1267 = 0x1002863
 
kbraille_dots_367 :: KeySym
kbraille_dots_367 = 0x1002864
 
kbraille_dots_1367 :: KeySym
kbraille_dots_1367 = 0x1002865
 
kbraille_dots_2367 :: KeySym
kbraille_dots_2367 = 0x1002866
 
kbraille_dots_12367 :: KeySym
kbraille_dots_12367 = 0x1002867
 
kbraille_dots_467 :: KeySym
kbraille_dots_467 = 0x1002868
 
kbraille_dots_1467 :: KeySym
kbraille_dots_1467 = 0x1002869
 
kbraille_dots_2467 :: KeySym
kbraille_dots_2467 = 0x100286a
 
kbraille_dots_12467 :: KeySym
kbraille_dots_12467 = 0x100286b
 
kbraille_dots_3467 :: KeySym
kbraille_dots_3467 = 0x100286c
 
kbraille_dots_13467 :: KeySym
kbraille_dots_13467 = 0x100286d
 
kbraille_dots_23467 :: KeySym
kbraille_dots_23467 = 0x100286e
 
kbraille_dots_123467 :: KeySym
kbraille_dots_123467 = 0x100286f
 
kbraille_dots_567 :: KeySym
kbraille_dots_567 = 0x1002870
 
kbraille_dots_1567 :: KeySym
kbraille_dots_1567 = 0x1002871
 
kbraille_dots_2567 :: KeySym
kbraille_dots_2567 = 0x1002872
 
kbraille_dots_12567 :: KeySym
kbraille_dots_12567 = 0x1002873
 
kbraille_dots_3567 :: KeySym
kbraille_dots_3567 = 0x1002874
 
kbraille_dots_13567 :: KeySym
kbraille_dots_13567 = 0x1002875
 
kbraille_dots_23567 :: KeySym
kbraille_dots_23567 = 0x1002876
 
kbraille_dots_123567 :: KeySym
kbraille_dots_123567 = 0x1002877
 
kbraille_dots_4567 :: KeySym
kbraille_dots_4567 = 0x1002878
 
kbraille_dots_14567 :: KeySym
kbraille_dots_14567 = 0x1002879
 
kbraille_dots_24567 :: KeySym
kbraille_dots_24567 = 0x100287a
 
kbraille_dots_124567 :: KeySym
kbraille_dots_124567 = 0x100287b
 
kbraille_dots_34567 :: KeySym
kbraille_dots_34567 = 0x100287c
 
kbraille_dots_134567 :: KeySym
kbraille_dots_134567 = 0x100287d
 
kbraille_dots_234567 :: KeySym
kbraille_dots_234567 = 0x100287e
 
kbraille_dots_1234567 :: KeySym
kbraille_dots_1234567 = 0x100287f
 
kbraille_dots_8 :: KeySym
kbraille_dots_8 = 0x1002880
 
kbraille_dots_18 :: KeySym
kbraille_dots_18 = 0x1002881
 
kbraille_dots_28 :: KeySym
kbraille_dots_28 = 0x1002882
 
kbraille_dots_128 :: KeySym
kbraille_dots_128 = 0x1002883
 
kbraille_dots_38 :: KeySym
kbraille_dots_38 = 0x1002884
 
kbraille_dots_138 :: KeySym
kbraille_dots_138 = 0x1002885
 
kbraille_dots_238 :: KeySym
kbraille_dots_238 = 0x1002886
 
kbraille_dots_1238 :: KeySym
kbraille_dots_1238 = 0x1002887
 
kbraille_dots_48 :: KeySym
kbraille_dots_48 = 0x1002888
 
kbraille_dots_148 :: KeySym
kbraille_dots_148 = 0x1002889
 
kbraille_dots_248 :: KeySym
kbraille_dots_248 = 0x100288a
 
kbraille_dots_1248 :: KeySym
kbraille_dots_1248 = 0x100288b
 
kbraille_dots_348 :: KeySym
kbraille_dots_348 = 0x100288c
 
kbraille_dots_1348 :: KeySym
kbraille_dots_1348 = 0x100288d
 
kbraille_dots_2348 :: KeySym
kbraille_dots_2348 = 0x100288e
 
kbraille_dots_12348 :: KeySym
kbraille_dots_12348 = 0x100288f
 
kbraille_dots_58 :: KeySym
kbraille_dots_58 = 0x1002890
 
kbraille_dots_158 :: KeySym
kbraille_dots_158 = 0x1002891
 
kbraille_dots_258 :: KeySym
kbraille_dots_258 = 0x1002892
 
kbraille_dots_1258 :: KeySym
kbraille_dots_1258 = 0x1002893
 
kbraille_dots_358 :: KeySym
kbraille_dots_358 = 0x1002894
 
kbraille_dots_1358 :: KeySym
kbraille_dots_1358 = 0x1002895
 
kbraille_dots_2358 :: KeySym
kbraille_dots_2358 = 0x1002896
 
kbraille_dots_12358 :: KeySym
kbraille_dots_12358 = 0x1002897
 
kbraille_dots_458 :: KeySym
kbraille_dots_458 = 0x1002898
 
kbraille_dots_1458 :: KeySym
kbraille_dots_1458 = 0x1002899
 
kbraille_dots_2458 :: KeySym
kbraille_dots_2458 = 0x100289a
 
kbraille_dots_12458 :: KeySym
kbraille_dots_12458 = 0x100289b
 
kbraille_dots_3458 :: KeySym
kbraille_dots_3458 = 0x100289c
 
kbraille_dots_13458 :: KeySym
kbraille_dots_13458 = 0x100289d
 
kbraille_dots_23458 :: KeySym
kbraille_dots_23458 = 0x100289e
 
kbraille_dots_123458 :: KeySym
kbraille_dots_123458 = 0x100289f
 
kbraille_dots_68 :: KeySym
kbraille_dots_68 = 0x10028a0
 
kbraille_dots_168 :: KeySym
kbraille_dots_168 = 0x10028a1
 
kbraille_dots_268 :: KeySym
kbraille_dots_268 = 0x10028a2
 
kbraille_dots_1268 :: KeySym
kbraille_dots_1268 = 0x10028a3
 
kbraille_dots_368 :: KeySym
kbraille_dots_368 = 0x10028a4
 
kbraille_dots_1368 :: KeySym
kbraille_dots_1368 = 0x10028a5
 
kbraille_dots_2368 :: KeySym
kbraille_dots_2368 = 0x10028a6
 
kbraille_dots_12368 :: KeySym
kbraille_dots_12368 = 0x10028a7
 
kbraille_dots_468 :: KeySym
kbraille_dots_468 = 0x10028a8
 
kbraille_dots_1468 :: KeySym
kbraille_dots_1468 = 0x10028a9
 
kbraille_dots_2468 :: KeySym
kbraille_dots_2468 = 0x10028aa
 
kbraille_dots_12468 :: KeySym
kbraille_dots_12468 = 0x10028ab
 
kbraille_dots_3468 :: KeySym
kbraille_dots_3468 = 0x10028ac
 
kbraille_dots_13468 :: KeySym
kbraille_dots_13468 = 0x10028ad
 
kbraille_dots_23468 :: KeySym
kbraille_dots_23468 = 0x10028ae
 
kbraille_dots_123468 :: KeySym
kbraille_dots_123468 = 0x10028af
 
kbraille_dots_568 :: KeySym
kbraille_dots_568 = 0x10028b0
 
kbraille_dots_1568 :: KeySym
kbraille_dots_1568 = 0x10028b1
 
kbraille_dots_2568 :: KeySym
kbraille_dots_2568 = 0x10028b2
 
kbraille_dots_12568 :: KeySym
kbraille_dots_12568 = 0x10028b3
 
kbraille_dots_3568 :: KeySym
kbraille_dots_3568 = 0x10028b4
 
kbraille_dots_13568 :: KeySym
kbraille_dots_13568 = 0x10028b5
 
kbraille_dots_23568 :: KeySym
kbraille_dots_23568 = 0x10028b6
 
kbraille_dots_123568 :: KeySym
kbraille_dots_123568 = 0x10028b7
 
kbraille_dots_4568 :: KeySym
kbraille_dots_4568 = 0x10028b8
 
kbraille_dots_14568 :: KeySym
kbraille_dots_14568 = 0x10028b9
 
kbraille_dots_24568 :: KeySym
kbraille_dots_24568 = 0x10028ba
 
kbraille_dots_124568 :: KeySym
kbraille_dots_124568 = 0x10028bb
 
kbraille_dots_34568 :: KeySym
kbraille_dots_34568 = 0x10028bc
 
kbraille_dots_134568 :: KeySym
kbraille_dots_134568 = 0x10028bd
 
kbraille_dots_234568 :: KeySym
kbraille_dots_234568 = 0x10028be
 
kbraille_dots_1234568 :: KeySym
kbraille_dots_1234568 = 0x10028bf
 
kbraille_dots_78 :: KeySym
kbraille_dots_78 = 0x10028c0
 
kbraille_dots_178 :: KeySym
kbraille_dots_178 = 0x10028c1
 
kbraille_dots_278 :: KeySym
kbraille_dots_278 = 0x10028c2
 
kbraille_dots_1278 :: KeySym
kbraille_dots_1278 = 0x10028c3
 
kbraille_dots_378 :: KeySym
kbraille_dots_378 = 0x10028c4
 
kbraille_dots_1378 :: KeySym
kbraille_dots_1378 = 0x10028c5
 
kbraille_dots_2378 :: KeySym
kbraille_dots_2378 = 0x10028c6
 
kbraille_dots_12378 :: KeySym
kbraille_dots_12378 = 0x10028c7
 
kbraille_dots_478 :: KeySym
kbraille_dots_478 = 0x10028c8
 
kbraille_dots_1478 :: KeySym
kbraille_dots_1478 = 0x10028c9
 
kbraille_dots_2478 :: KeySym
kbraille_dots_2478 = 0x10028ca
 
kbraille_dots_12478 :: KeySym
kbraille_dots_12478 = 0x10028cb
 
kbraille_dots_3478 :: KeySym
kbraille_dots_3478 = 0x10028cc
 
kbraille_dots_13478 :: KeySym
kbraille_dots_13478 = 0x10028cd
 
kbraille_dots_23478 :: KeySym
kbraille_dots_23478 = 0x10028ce
 
kbraille_dots_123478 :: KeySym
kbraille_dots_123478 = 0x10028cf
 
kbraille_dots_578 :: KeySym
kbraille_dots_578 = 0x10028d0
 
kbraille_dots_1578 :: KeySym
kbraille_dots_1578 = 0x10028d1
 
kbraille_dots_2578 :: KeySym
kbraille_dots_2578 = 0x10028d2
 
kbraille_dots_12578 :: KeySym
kbraille_dots_12578 = 0x10028d3
 
kbraille_dots_3578 :: KeySym
kbraille_dots_3578 = 0x10028d4
 
kbraille_dots_13578 :: KeySym
kbraille_dots_13578 = 0x10028d5
 
kbraille_dots_23578 :: KeySym
kbraille_dots_23578 = 0x10028d6
 
kbraille_dots_123578 :: KeySym
kbraille_dots_123578 = 0x10028d7
 
kbraille_dots_4578 :: KeySym
kbraille_dots_4578 = 0x10028d8
 
kbraille_dots_14578 :: KeySym
kbraille_dots_14578 = 0x10028d9
 
kbraille_dots_24578 :: KeySym
kbraille_dots_24578 = 0x10028da
 
kbraille_dots_124578 :: KeySym
kbraille_dots_124578 = 0x10028db
 
kbraille_dots_34578 :: KeySym
kbraille_dots_34578 = 0x10028dc
 
kbraille_dots_134578 :: KeySym
kbraille_dots_134578 = 0x10028dd
 
kbraille_dots_234578 :: KeySym
kbraille_dots_234578 = 0x10028de
 
kbraille_dots_1234578 :: KeySym
kbraille_dots_1234578 = 0x10028df
 
kbraille_dots_678 :: KeySym
kbraille_dots_678 = 0x10028e0
 
kbraille_dots_1678 :: KeySym
kbraille_dots_1678 = 0x10028e1
 
kbraille_dots_2678 :: KeySym
kbraille_dots_2678 = 0x10028e2
 
kbraille_dots_12678 :: KeySym
kbraille_dots_12678 = 0x10028e3
 
kbraille_dots_3678 :: KeySym
kbraille_dots_3678 = 0x10028e4
 
kbraille_dots_13678 :: KeySym
kbraille_dots_13678 = 0x10028e5
 
kbraille_dots_23678 :: KeySym
kbraille_dots_23678 = 0x10028e6
 
kbraille_dots_123678 :: KeySym
kbraille_dots_123678 = 0x10028e7
 
kbraille_dots_4678 :: KeySym
kbraille_dots_4678 = 0x10028e8
 
kbraille_dots_14678 :: KeySym
kbraille_dots_14678 = 0x10028e9
 
kbraille_dots_24678 :: KeySym
kbraille_dots_24678 = 0x10028ea
 
kbraille_dots_124678 :: KeySym
kbraille_dots_124678 = 0x10028eb
 
kbraille_dots_34678 :: KeySym
kbraille_dots_34678 = 0x10028ec
 
kbraille_dots_134678 :: KeySym
kbraille_dots_134678 = 0x10028ed
 
kbraille_dots_234678 :: KeySym
kbraille_dots_234678 = 0x10028ee
 
kbraille_dots_1234678 :: KeySym
kbraille_dots_1234678 = 0x10028ef
 
kbraille_dots_5678 :: KeySym
kbraille_dots_5678 = 0x10028f0
 
kbraille_dots_15678 :: KeySym
kbraille_dots_15678 = 0x10028f1
 
kbraille_dots_25678 :: KeySym
kbraille_dots_25678 = 0x10028f2
 
kbraille_dots_125678 :: KeySym
kbraille_dots_125678 = 0x10028f3
 
kbraille_dots_35678 :: KeySym
kbraille_dots_35678 = 0x10028f4
 
kbraille_dots_135678 :: KeySym
kbraille_dots_135678 = 0x10028f5
 
kbraille_dots_235678 :: KeySym
kbraille_dots_235678 = 0x10028f6
 
kbraille_dots_1235678 :: KeySym
kbraille_dots_1235678 = 0x10028f7
 
kbraille_dots_45678 :: KeySym
kbraille_dots_45678 = 0x10028f8
 
kbraille_dots_145678 :: KeySym
kbraille_dots_145678 = 0x10028f9
 
kbraille_dots_245678 :: KeySym
kbraille_dots_245678 = 0x10028fa
 
kbraille_dots_1245678 :: KeySym
kbraille_dots_1245678 = 0x10028fb
 
kbraille_dots_345678 :: KeySym
kbraille_dots_345678 = 0x10028fc
 
kbraille_dots_1345678 :: KeySym
kbraille_dots_1345678 = 0x10028fd
 
kbraille_dots_2345678 :: KeySym
kbraille_dots_2345678 = 0x10028fe
 
kbraille_dots_12345678 :: KeySym
kbraille_dots_12345678 = 0x10028ff
 
