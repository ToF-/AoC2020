module Day19b
    where
import Text.ParserCombinators.ReadP
import Data.Char


-- 8: 42 | 42 8
-- 11: 42 31 | 42 11 31

rule8 = rule42 +++ (rule42 >> rule8)
rule11 = (rule42 >> rule31) +++ (rule42 >> rule11 >> rule31)
rule124 = (rule72 >> rule26) +++ (rule58 >> rule91)
rule76 = (rule72 >> rule90) +++ (rule58 >> rule73)
rule89 = (rule58 >> rule135) +++ (rule72 >> rule25)
rule43 = (rule58 >> rule30) +++ (rule72 >> rule98)
rule130 = rule58 >> rule58
rule87 = (rule135 >> rule72) +++ (rule100 >> rule58)
rule24 = (rule72 >> rule5) +++ (rule58 >> rule78)
rule61 = (rule84 >> rule72) +++ (rule71 >> rule58)
rule68 = (rule49 >> rule72) +++ (rule91 >> rule58)
rule103 = (rule126 >> rule58) +++ (rule124 >> rule72)
rule132 = (rule58 >> rule17) +++ (rule72 >> rule37)
rule75 = (rule72 >> rule89) +++ (rule58 >> rule50)
rule99 = (rule72 >> rule72) +++ (rule58 >> rule58)
rule28 = (rule58 >> rule19) +++ (rule72 >> rule65)
rule2 = (rule9 >> rule72) +++ (rule67 >> rule58)
rule113 = (rule58 >> rule100) +++ (rule72 >> rule130)
rule66 = (rule129 >> rule58) +++ (rule70 >> rule72)
rule74 = (rule72 >> rule116) +++ (rule58 >> rule56)
rule45 = (rule72 >> rule82) +++ (rule58 >> rule38)
rule70 = (rule137 >> rule58) +++ (rule12 >> rule72)
rule78 = (rule72 >> rule58) +++ (rule58 >> rule58)
rule115 = (rule78 >> rule58) +++ (rule27 >> rule72)
rule48 = (rule58 >> rule121) +++ (rule72 >> rule95)
rule23 = (rule120 >> rule58) +++ (rule93 >> rule72)
rule44 = (rule58 >> rule25) +++ (rule72 >> rule130)
rule17 = (rule58 >> rule27) +++ (rule72 >> rule91)
rule65 = (rule102 >> rule72) +++ (rule46 >> rule58)
rule19 = (rule72 >> rule84) +++ (rule58 >> rule35)
rule79 = (rule29 >> rule72) +++ (rule127 >> rule58)
rule13 = (rule58 >> rule88) +++ (rule72 >> rule136)
rule6 = (rule15 >> rule72) +++ (rule130 >> rule58)
rule81 = rule58 >> rule72
rule100 = (rule117 >> rule72) +++ (rule58 >> rule58)
rule116 = (rule78 >> rule72) +++ (rule49 >> rule58)
rule136 = (rule20 >> rule72) +++ (rule50 >> rule58)
rule97 = (rule53 >> rule72) +++ (rule85 >> rule58)
rule107 = (rule58 >> rule27) +++ (rule72 >> rule5)
rule0 = rule8 >> rule11
rule49 = (rule58 >> rule72) +++ (rule72 >> rule117)
rule88 = (rule105 >> rule58) +++ (rule21 >> rule72)
rule83 = (rule69 >> rule72) +++ (rule5 >> rule58)
rule112 = (rule58 >> rule32) +++ (rule72 >> rule2)
rule47 = (rule62 >> rule72) +++ (rule118 >> rule58)
rule59 = (rule58 >> rule134) +++ (rule72 >> rule101)
rule60 = (rule132 >> rule72) +++ (rule103 >> rule58)
rule55 = (rule58 >> rule44) +++ (rule72 >> rule92)
rule67 = (rule72 >> rule15) +++ (rule58 >> rule100)
rule96 = (rule81 >> rule72) +++ (rule135 >> rule58)
rule121 = (rule25 >> rule58) +++ (rule26 >> rule72)
rule39 = (rule58 >> rule81) +++ (rule72 >> rule25)
rule82 = rule81 >> rule58
rule27 = (rule72 >> rule72) +++ (rule117 >> rule58)
rule85 = (rule58 >> rule119) +++ (rule72 >> rule68)
rule53 = (rule72 >> rule39) +++ (rule58 >> rule80)
rule91 = (rule72 >> rule58) +++ (rule72 >> rule72)
rule129 = (rule58 >> rule133) +++ (rule72 >> rule3)
rule18 = (rule58 >> rule69) +++ (rule72 >> rule25)
rule120 = (rule110 >> rule58) +++ (rule36 >> rule72)
rule114 = (rule58 >> rule87) +++ (rule72 >> rule16)
rule34 = (rule72 >> rule40) +++ (rule58 >> rule45)
rule29 = (rule58 >> rule123) +++ (rule72 >> rule59)
rule36 = rule15 >> rule117
rule133 = (rule58 >> rule107) +++ (rule72 >> rule113)
rule109 = (rule58 >> rule58) +++ (rule58 >> rule72)
rule46 = rule100 >> rule117
rule41 = (rule109 >> rule58) +++ (rule91 >> rule72)
rule26 = (rule72 >> rule72) +++ (rule58 >> rule72)
rule72 = char 'a'
rule21 = (rule58 >> rule78) +++ (rule72 >> rule130)
rule117 = rule58 +++ rule72
rule111 = (rule58 >> rule5) +++ (rule72 >> rule81)
rule95 = (rule58 >> rule27) +++ (rule72 >> rule109)
rule4 = (rule97 >> rule72) +++ (rule28 >> rule58)
rule135 = (rule58 >> rule117) +++ (rule72 >> rule58)
rule37 = rule58 >> rule27
rule1 = (rule58 >> rule99) +++ (rule72 >> rule25)
rule52 = (rule130 >> rule58) +++ (rule109 >> rule72)
rule106 = (rule5 >> rule72) +++ (rule99 >> rule58)
rule12 = (rule111 >> rule58) +++ (rule68 >> rule72)
rule42 = (rule94 >> rule58) +++ (rule79 >> rule72)
rule77 = (rule125 >> rule72) +++ (rule86 >> rule58)
rule98 = (rule58 >> rule91) +++ (rule72 >> rule109)
rule62 = (rule58 >> rule122) +++ (rule72 >> rule52)
rule119 = (rule109 >> rule72) +++ (rule135 >> rule58)
rule108 = (rule15 >> rule72) +++ (rule25 >> rule58)
rule10 = (rule78 >> rule58) +++ (rule15 >> rule72)
rule5 = rule72 >> rule58
rule3 = (rule72 >> rule33) +++ (rule58 >> rule37)
rule57 = (rule58 >> rule51) +++ (rule72 >> rule7)
rule7 = (rule72 >> rule43) +++ (rule58 >> rule74)
rule38 = (rule58 >> rule91) +++ (rule72 >> rule15)
rule20 = (rule26 >> rule58) +++ (rule130 >> rule72)
rule22 = (rule58 >> rule99) +++ (rule72 >> rule15)
rule105 = (rule15 >> rule72) +++ (rule99 >> rule58)
rule25 = rule72 >> rule72
rule35 = (rule72 >> rule99) +++ (rule58 >> rule91)
rule56 = (rule78 >> rule72) +++ (rule99 >> rule58)
rule63 = (rule114 >> rule72) +++ (rule55 >> rule58)
rule86 = (rule23 >> rule72) +++ (rule13 >> rule58)
rule54 = (rule47 >> rule58) +++ (rule112 >> rule72)
rule50 = (rule72 >> rule5) +++ (rule58 >> rule99)
rule126 = (rule58 >> rule81) +++ (rule72 >> rule26)
rule127 = (rule63 >> rule72) +++ (rule60 >> rule58)
rule9 = (rule72 >> rule27) +++ (rule58 >> rule15)
rule137 = (rule10 >> rule58) +++ (rule96 >> rule72)
rule15 = rule117 >> rule117
rule128 = (rule66 >> rule58) +++ (rule57 >> rule72)
rule58 = char 'b'
rule14 = (rule18 >> rule72) +++ (rule116 >> rule58)
rule92 = (rule58 >> rule25) +++ (rule72 >> rule100)
rule40 = (rule83 >> rule58) +++ (rule131 >> rule72)
rule31 = (rule77 >> rule58) +++ (rule128 >> rule72)
rule69 = (rule58 >> rule72) +++ (rule72 >> rule58)
rule125 = (rule34 >> rule58) +++ (rule76 >> rule72)
rule110 = rule117 >> rule99
rule94 = (rule72 >> rule4) +++ (rule58 >> rule54)
rule102 = (rule72 >> rule25) +++ (rule58 >> rule109)
rule30 = (rule58 >> rule91) +++ (rule72 >> rule5)
rule84 = rule99 >> rule117
rule93 = (rule50 >> rule72) +++ (rule6 >> rule58)
rule80 = (rule72 >> rule100) +++ (rule58 >> rule91)
rule64 = (rule58 >> rule5) +++ (rule72 >> rule130)
rule51 = (rule58 >> rule61) +++ (rule72 >> rule48)
rule122 = (rule5 >> rule58) +++ (rule25 >> rule72)
rule16 = (rule58 >> rule130) +++ (rule72 >> rule78)
rule90 = (rule22 >> rule58) +++ (rule106 >> rule72)
rule131 = (rule58 >> rule91) +++ (rule72 >> rule130)
rule123 = (rule14 >> rule72) +++ (rule75 >> rule58)
rule33 = (rule58 >> rule15) +++ (rule72 >> rule26)
rule71 = (rule27 >> rule72) +++ (rule69 >> rule58)
rule73 = (rule104 >> rule58) +++ (rule24 >> rule72)
rule32 = (rule72 >> rule126) +++ (rule58 >> rule41)
rule134 = (rule72 >> rule1) +++ (rule58 >> rule35)
rule101 = (rule72 >> rule84) +++ (rule58 >> rule115)
rule104 = rule130 >> rule58
rule118 = (rule64 >> rule58) +++ (rule108 >> rule72)


messages = ["baabababbaababbbaabbbbab"
           ,"baabababbbabaabaabbbbbbb"
           ,"bbbabaaababbbbaaababbabbaabbabba"
           ,"bbbbbbaaaababbaaaababaaabbaaababaaababba"
           ,"bbabbaaaabbbabaaaabbbbbbabbbbbaa"
           ,"aaaabaaabbbabbaaaabbbbbabaabbbaabbbaaaabbababbbb"
           ,"bbbabababbbbbbbbbbbbabbbababbaba"
           ,"aabbbbaaabbabbabaaaababaabbbabbbbabbaaabaaaaabbbbaaababbabbbbbbabbbbaaababbbbaab"
           ,"bbababbaaabbbbbbabaaaabbbbbbabba"
           ,"babaaaabbaaabbbabaaaaabb"
           ,"bbbabaaabaaabbbabababbaaabbbbbabababababaaaaaaba"
           ,"bbaabaabbbaaaabaabaaabaa"
           ,"bbaabababbaaabbbaaaaabaabababbbbbaaabbaa"
           ,"babbbbaaaaaaaabbabbabbba"
           ,"bbbbbbabababbabbbbbbbbba"
           ,"bbaaababbaaabbbbaababbbababaaaabaabbbaaaabbbabbabaaabbab"
           ,"bbaaaabaaabbbaaabbaaabaa"
           ,"baabbbaababbbbaaaaabbbbbbaaababbbbbbaaba"
           ,"abbabbbabaaaaaaabbabaaabaaabababbabaaabaaabbbaaa"
           ,"aababaaababbbaabababbabbabbbabbbbaaaababaabbabab"
           ,"ababbbbaaaaaaaabababaabb"
           ,"ababbbaaaaaabbabbaaabbabbabbaabbaabaaaab"
           ,"bbaabaabaaabbaaabbbbbbbb"
           ,"babaababbbbbbaaabbababba"
           ,"bbaabbaaaaaabbbbabbbabbbabaabbabababaaba"
           ,"bbbbbaaababbabaabbbaabbaaabbbbababbabaabaaaaaabaaabababaaaaaaaabaaaaababbbbbaabababbabbb"
           ,"aabbbabababbaaabbbbaaaabbabbbbbbabaabaab"
           ,"abbbabbaabbbababbbababbabaabaaabbababbab"
           ,"baaaabaaaaabbababbaaaaaabababbbbabbbbababaaaaaaaabbbabab"
           ,"aabaabababbabbbbbaabbaabbbaabbba"
           ,"aaaabbbbabbabbabbaabbaabbaaaababababaaaabaabaabbabbbbbaabaaaaaba"
           ,"bbbaaaabbaabbaababbabbbaabaababbbbaabbbbbbaabaaaabbbbaabaaaaabba"
           ,"babbaabbaaaabbababaaabbabababbaaabbbababbbbbaabb"
           ,"aabbbbaaabbbabbbabbababa"
           ,"aaabaabaabbaaaabababaaabababbaabbabaaabbabaababaabbbaaab"
           ,"aabbbabbbaababbbbbaaabaaaabaabaabaabaabbbaabbbaabbbaabba"
           ,"bbaabbaababbbaabaaaaaabbbbbbbbba"
           ,"ababbabbabbbaabbabaababaaaaaaaaabbbaabaabbbabbba"
           ,"bbbabbabbabbbabbabbaaaaabbabbbabbbaaababababbabbbaaaaaaaaaababaaaabaabaaabbbababbbbaaaab"
           ,"babaaaabababaaaabbabaaaababaababbabaabbabaabaaabaabbabba"
           ,"aaabaabaabbbaabbbbbbabbababbabab"
           ,"baababbbbabaabbaabbbaaaaabbabbbabbaabaabbababbaabbabbbbaabbbbbbb"
           ,"ababbaababbabbbbbaababaaababbaba"
           ,"baabbbabaaabbaababaaaabbbabbbaaaaaaaabaaaabbbbabababaabbabaaaabb"
           ,"bbbaaababbbbbbaabbbbabbb"
           ,"bbbaaaabaabaababaaabbaaaabbabbaaaaaaabbbaaabbabbbbabbaababbaaababaaabbaa"
           ,"aabbbabaaaaaabbbabbabbba"
           ,"aaaaaaabababbbabaabbbbba"
           ,"aaaaaaaaababbbaababababbbbbabbbbabababaaabababaabbbbbaba"
           ,"bbaaaaabababaababbbaabbababbaaababbabababbababbaabbababbabaababaaabababbbbbbaaba"
           ,"bbaaaabbaaaaaaaababbaaaa"
           ,"aaaaabaababaababaaababaa"
           ,"babaabbbbaaabbbabbabbbbbbaaabbababaaabaababbaaaa"
           ,"bbbbbbaabababbaaaaabbbbabaabbaba"
           ,"abaaabbbaababbaaaababbbbbbaaaaababbabaaabbbaababaaababaa"
           ,"abbabbbabaaabaabaabbbabbbabbbabaababaaaaaaaaaaabbbbabaaaabbababbbaaaaaaababbabbbabbbbaab"
           ,"aababbaaaaabaababbbaaabababbababbbbaabbbbabaabbbabaabbaa"
           ,"bbaabababbbbaababbaaabbabaaaaabbbabababa"
           ,"aabbbabaabbbaabaaabbbabaabaababababbaaba"
           ,"bbabbaabbbbbbbbabbbbbaaaaabbaaabbbbabbbbbaaabbabbbbbbbbbbbbbbaababbaaabbaaaababbbbbbaaab"
           ,"abbbababbbaaaaaabbbabbabbbbabbabbbabaabb"
           ,"aababbaaabbbaabbababbaaa"
           ,"abbabbbbbbbbaababaaabbbb"
           ,"aaabbbaaaaabbbabbabbaaaa"
           ,"ababbbbbabbabaabaaaaabaaabbabbaabbbbbaaa"
           ,"aaaabbbabbbbababbbababbaabbbaabbaaababba"
           ,"aaabbbabbbaabaababbbaaabbaaaababaabbabaababbabaa"
           ,"baaaabaabaaabaabababaaba"
           ,"abbbabbbabbbabbaaaabbaab"
           ,"bbabbabbbaaaababbbaaabbaabbabbbbaabbbaaaabbbaabbaabbababababaaba"
           ,"abbabbaaaababababaabbbaa"
           ,"ababbbaabaaabbbabbbabbab"
           ,"bbaaabbbbaaaaaababbbaababbabbbaabbaaaabb"
           ,"bbabbabbaabbababbaabbabaaaabaaaaabababaaaaabbaab"
           ,"bbaababaaaabbbaaaabbbbabbbabaabb"
           ,"bbababbaabaabbbababaababaabaaaab"
           ,"bababaaaaaaabbbabbbbaabaabbbabaababbbabbababaaba"
           ,"babbaabbbbbbababbbabbbbbbaaaaaabbabbabaaaaabbabb"
           ,"babbbabbabaaabbabbbbaaaa"
           ,"aaaabbabbbababbabbaaaabaaaabbbaa"
           ,"abbbaabaaababaaaabbabbbbbbbaaaaabbabaabbababbaaabbbbbbbabaabaabb"
           ,"abbbabaaababaaaaaabaababbbababbaaabbabaabbbbaaabbbbbbaab"
           ,"babbaabbbbbaababbaaabbbbabbaaaaa"
           ,"babbbbbbbbbabaaabaabbbba"
           ,"bbbaaabaabaaaaabbaabbababbaabbabbabbababbabbbabaaabbabab"
           ,"aabbbaaaababbaabbbabaaab"
           ,"bbbbbbaaababbbaabaababba"
           ,"bbabbbbbbbabaababbbabbba"
           ,"babbabaababbbaaababbabababbbaabbbbaaabbbaaaabbababaababb"
           ,"ababaaaaaaaaaaabbbaaaabbbaabbbbb"
           ,"abbbaaaaaaaaabbaababaabb"
           ,"baabaabaaabbbabaabbabbabaabbbbba"
           ,"abaaabbbbbabbbbbbbbaabbbbbaaaabbbbbaabbbbbabbbababbaabaa"
           ,"aaabbbaaababbbbabbbbabbabaabaabb"
           ,"abbabbbbbababaaababbabababbabbbbbaaaaaabbbaabaaaaabaaabb"
           ,"ababbabbaabbaaabaaababbb"
           ,"bbbabaaaabaaaaabbaabbaabbbaababaaaaaababaaabaaabbbbbabaa"
           ,"babbbababbbbbbabaaabaabababbbbbbbaaabbbb"
           ,"aababaaaababbbaaababbbaababbbaaabbbaabbaaababbba"
           ,"ababbbbaaabbbaaaaabaababaaaaaaaaabbababababbaaba"
           ,"aabbaaababbabbaaaaabbbaa"
           ,"bbbbaaabbaabbababaabbbabbbabaaabbbabbabababaabab"
           ,"babaababbbbbabababbabbbbbbbbaaaa"
           ,"abbbabaaabaaabbbaaabbbaaaaabbabaabbabbaaaabbaabaaabaaabaabbbbabbbabaaabbaababbaabbabaabb"
           ,"aaabbbbaabbbababababbabbabaaabbabaabaabb"
           ,"aaabbbaabbbbababbbaababb"
           ,"baabababbbbabbbabbaabbababbbaaabaaababbb"
           ,"bbbabaaaabaaaaabbbababab"
           ,"baaaaabbbabbabbbbbaaaabaababbbbaabaabbabbbaaabbaababaaba"
           ,"abbaaaabaaabbaaababbabababbbaababbbbbbaabbbbbaaa"
           ,"baabbbabaababaaaabbababa"
           ,"aabbbabbabaaaaaabbbbbaba"
           ,"aabbbbaaaaaaababbaabbbaababbabbbaaababbbabaabbbbbabbaaaa"
           ,"aabbaabbbabaaaabbbabbaab"
           ,"aabbaaaaabababbbabaaaaaabbbababbaababababbaabbabaaabaaaababbbbab"
           ,"bababbaaababaaaabbbbbbbb"
           ,"aaaaababbbaaaabbbbaabbab"
           ,"baabbaaaaabbbaaabaaababa"
           ,"bbbabbbabbaabaaabbbbaabbbbabbbaabbbaaaababbababbbaabababbaabaaab"
           ,"bbababababbababbbaaabbaaabbabababbbbbbaabaababaa"
           ,"aababbaabaaaabbbabababaa"
           ,"aaabbaaabbbaaaabbbbbaaaa"
           ,"bbabbabbabbaaaababbababa"
           ,"bbbabaaaaaabbaaaaabbabbbaaabbabaabbabaaa"
           ,"aaaaaaaabbaababaaabbbbaabaabababbbbbbaab"
           ,"babbaabaababaabaabababaabbbbbbba"
           ,"aababaaaababbbbbbabbabababababaaabbbaaab"
           ,"baabbbaabbbbbbbbabbaabbb"
           ,"baababbbabbabbabaaaaaababaaabaaa"
           ,"ababbbababbbbbabbaabbbaa"
           ,"bbaababaaabbbbaaababaaababbababa"
           ,"bbabbaaaabbabbbbaaabaaab"
           ,"abbbabbabbabaabaaaaaaabbabbbbbbbbbbbbaaa"
           ,"abbaaabbbbaaabbbbaababbbaaaabaab"
           ,"bbaabababaaabbbaabbaabbb"
           ,"ababbabbaaabaabaaaaaaaabababaaababaaabbbaabbabaabbbbbaba"
           ,"aababababaaaababbabaaaababaaabbbbaaaaaba"
           ,"babaaabaabbbbbbbabbbbbbbbaababaabbaaabaa"
           ,"bababbbabaabbbabaaabbbaaaaabaababaabbbaabbbbbbba"
           ,"ababbbabbbaaaabaabaabaaa"
           ,"ababbbbaaabbaaabaababbbbaaaaaaaaabbababb"
           ,"bbaabaabaaaabbabaaaaaaabbabbaaaaabbbbbbb"
           ,"aabaaabbababbbabaaaabaabbbaababb"
           ,"babbbbabaaaabababaababbabaaaabbabbabaaabaaabbbbbbbaabbbb"
           ,"babbaababbbbbbbabbabaabbbbabbaaaababaaaa"
           ,"baabaabababbbbabbaaabaaaabbbaabaaabaabbabbbbabbbabaaaabababaaaabbbbbbabb"
           ,"bbbabaabaabbbabbbaabbaabaabaabba"
           ,"babbbababbabbabbabbbaabaaaaabbbbbabaabbbabaaaabbbaaabaaa"
           ,"aaabbbaababbbaaaaaabbbbaaaaaabbbaaaabbaaaababbbabbbabbba"
           ,"babbaabbababbbbaaababbaabbaaaaaa"
           ,"abaababaabbbabaaaaabbaba"
           ,"abbabbabaabaaaaaaaabbaba"
           ,"bbbbbbaaababbbbaabbaaabbaababbbaaaababbb"
           ,"abbaaabbbbbbababaabaaaaaaaaaababbbbabbaa"
           ,"abbbabaaaabbaabbaaabababbbbbaaaa"
           ,"bbbababbaababbbbbbaaabbbbaabbababaaaaabb"
           ,"abbaababaabbbabbabababab"
           ,"ababbaababaaabbbbababaaaaabababaaaabaabb"
           ,"aaaaababbabbabaababbabba"
           ,"aabbaababbaabaaaaaabbabababaaaaa"
           ,"bbabbabbaabaabbbbababaaabbbababbababaabb"
           ,"babaaababbababbabbbbbbaabbbaaaaabbaaaabbbaaaaabb"
           ,"aaabaabaaaaaabbbaaaabababaaabbbbbbbbabbbbaababababaabbbaabaaabaabbabbbba"
           ,"baabababbabaaaabababaaabbaabbbba"
           ,"abaaaaaababbbaababbbbbba"
           ,"baabbaaabaabbbabbbabbbaa"
           ,"ababbbbbabaababaaabbabba"
           ,"abbaaabbbabbaaabbbbbbaaa"
           ,"ababaaaaaabbaabbbbbaaaaaabaabbbabbbabaabbababbabbbbbaaababbbbaab"
           ,"aabaaaaaaaaabaaabababbbaababaabaaaaabbababaabbababbaaaabbbabaaba"
           ,"babaabababababbbaaabbbaaaabaaaabaaaabbbbaaababaaaababbaabaabaaabababaabb"
           ,"ababbaabbbabbaababbababa"
           ,"ababaaabbbaaabaabbbababbbaabaaabaabaaabb"
           ,"aabbbabaaabaabababaaabbaaaabbabbaaababbb"
           ,"baaabbabbbaaaababaaabaabbaaaabbbabbaaaabaaaabaabbaaabaaa"
           ,"baaaababbaabbbbaaaaabbba"
           ,"bbababbaaabaababbbbbbaaa"
           ,"bbbaaabaabaaabbabaabaaaa"
           ,"aabbbbaabaababbaaaabbaab"
           ,"ababababbbbabbbaabbbbabbbaaababb"
           ,"abbaaabbbaaaabbabbbbbaba"
           ,"aababbaaaaaaababbabbaaba"
           ,"abbaaabababbaabaaaaaaabaabbaabaabaabaaababaaaaba"
           ,"abaabbbababbbbaabbbbaabb"
           ,"abaaabbaaaaabbbbaaabaaaa"
           ,"bababbbbbaabbbbbbababbababbbbaba"
           ,"abbbabbbabbaaaabbabbbaaaaaabaabababbbbbaabaaabab"
           ,"ababbbbbbabbbbabaaaaabba"
           ,"baaabbabbaaaaaaaaabbbbaabbbbbaba"
           ,"aabaaabbabbaabbabaabaaabbabbababaaabbaabaababaab"
           ,"abbbbabbaaabbaaaabbbbbaaababbbbbbbbaaabbbabbaaab"
           ,"ababbaabbbbabaaaabaabbaa"
           ,"abababbababbabaabbaaaabaaaaaaabbaaaaaaabaababbababababbbaababbba"
           ,"aaaaababbaababaaabbbaaababbbbaabbbbabbbaababbaaa"
           ,"bbaaaaabbabbaabbbbbbbbba"
           ,"ababbabbabaaabbbaabababa"
           ,"bababbbaabbaaaaabbbbbaab"
           ,"abaaaaaabababaaabaabaabaaaaabaaa"
           ,"abaaabbaaababaaabbbbbabaababbbbbababaabababbbbbaabbbabbbabbaaabaababbaabbaabbabb"
           ,"ababaaaabbaabaaaabbabbba"
           ,"bbbabaaabbbabaaaabaaaaabbababbaaabababbb"
           ,"bbabbabbbaaaaaabbbbbbbbb"
           ,"bbbbbbaabbaabaabaaabbabb"
           ,"bbbabaaabbabbabbaabbabba"
           ,"aabaababaaaaaaaaaabbbabaabbbabbaabbbbaabbbbbaaaa"
           ,"abbaaabbaaabbbbaaababbab"
           ,"abaabababbbabaabbaaaabaaaaabbabbabaabaaa"
           ,"babbbaaabaabbaabbbbaabbbabbbabbababbbababbbabbaa"
           ,"aaaaaabbbbbaaababbaababb"
           ,"abbaaabbbbbbabbabbaabbab"
           ,"bbbbabbaabaababaaaaabababaaaabba"
           ,"baaababbbbaaabaaaabababbbbaabbbbbbababab"
           ,"babbbbababbbabbbaaabaaab"
           ,"bbaaabbbbbbaabaaaabbbbbbababbbabaaababbabbbbbaba"
           ,"bbabababbababbbabbbbbaabbaababbbbbaababaaabbabbababbaaaaababbbbabaaabaabbbbabbaaaabbbaba"
           ,"bbaabbbababbbbbbbaaabababbaaabaabbababbaabaaabaababbbaabbbbbbbba"
           ,"baaaabababbaaaaabbabbbbabbbababb"
           ,"aababbbbbabbaabbbbaaaaabaabbabbabaaababa"
           ,"babbbbaaababbbabbbbbabababaaaaaaabbaaaba"
           ,"babbaabbabbabbabaaabbbaabababaaababbabbaaababaabbabbabbb"
           ,"bbaabaabaabaabbbababaabb"
           ,"aaaabbbababbababbabbabaaaaabbbbabbbababbbbbabbbbbaaaaabb"
           ,"aabbbbaababbbbababbbbabbbaaabaaa"
           ,"aaabbbbbabbbaaabbaababaa"
           ,"babaaaabaabbbaaabbaabbaa"
           ,"aaaaabbbaaabbbbabaaabbabbabbbababbabbbab"
           ,"bbbbaababaabababbabbabbb"
           ,"abbaababbbaaaababbbaaabaaabbabaa"
           ,"ababbaabaaabbbabbaabaaab"
           ,"baababbbabaaaaaaabbbabbabbabbabaaababbba"
           ,"baaabbbaaabbbaaababbabaabbbbbaba"
           ,"bababbbbababbbbababaababbaabbbaabaaabbbbababaababbbbabbbbabaaababababbaababbbbaa"
           ,"abbaaaaabbbbbbababbbbaaa"
           ,"bbaabbababbbbaaaabaabababaaaabbababababbbbababbb"
           ,"abbbabbaabbaaabbaaabbaba"
           ,"aaaabbbababbbbabaababaab"
           ,"bbaabbaaabaabbbababaaaabbbaabbbabbbabbab"
           ,"babbbbbbbbababbaabaababb"
           ,"bbbbabbbaaabbbbbaaaabaabaababaababaabbbb"
           ,"babbbabbaababbaabaaaaabb"
           ,"abbbabbbaabbbabbbbaaaaba"
           ,"babababbaabbabbaaabbbbab"
           ,"baaaabaaaaabaabaabbaaaba"
           ,"ababbaaabbbabbbbabababbabbbbabbb"
           ,"babaabababaaabbabababbaaaabaabaa"
           ,"bababbaaabbabbabaaababaa"
           ,"baaaabaaaabbbbbbabbabbba"
           ,"aabbaababaabbbabbabbbbbbbabbbabbaaaaabba"
           ,"abbaabbbabbbbbaabbaabbbbbbbbaababababbabababbaab"
           ,"ababbbbbabaaaaabbbbaaababbbaabbbaaabaaaabaaaaababaabbbaa"
           ,"babbababababaaaaaababbab"
           ,"babaaabbabaaaaaabbbbaaaa"
           ,"bbbbaaaabbbabbbabaabaabbbbbabbab"
           ,"bababbaaabbbaababababaaaabbabbbbbbbaababbabbabba"
           ,"ababbbababaaaaababbabaaa"
           ,"bbbbaababababaaaababaabb"
           ,"baaaabbbaabababababbabba"
           ,"aabbbabbaaabbbabbbaaaaabbbabbaabaaaaaaaabbaaaabaabbaabaa"
           ,"aabaababaababbabbbbaababaabbbbabaaabbbbbaaabaaabbababbab"
           ,"abbabaabbbabbbaaabbaabba"
           ,"aababaaaaababaaabbaaaabbabbbbbaaabababaa"
           ,"baabbaababbbbaabbabbbaaaabbabaaaaaaabbbabbbaaabaaaabaaabbbaabbaaabbaabbbaabbabaaaaaabaab"
           ,"babbbabbbaababbbabbabaaa"
           ,"aaababababbbaabbabbabbba"
           ,"babbbbbbaaabbaaabbbbbaab"
           ,"abbbaabbaaaaabbbbbabbbab"
           ,"bbaabababaabbbabbabababa"
           ,"bbabbbbabbbaaaabbaabaabaaabbbabbbbbababb"
           ,"abbabbabbbbbaabaababbbbbbbababbaabbbbbbaaabaaabbbbaabbba"
           ,"bababaabaababaaabbabbbbb"
           ,"babbaabbbbaabaabbbaabbbabaabbaaabbbbaaabaabbabba"
           ,"bbabbaabababbbbabbbabaaa"
           ,"baabbaabaaaaabaabbaaabab"
           ,"aaabababbabbbaaabaaaabbaaaabbbaabaaaabbbabbbbbabaaaabaababbbbaab"
           ,"baabbbabaabababaaaaabaabaaaaabaaaaaaabbaaabbbaabaabbbbaabaaaabba"
           ,"bbaabbaaaabbbaaaabaaabab"
           ,"baaaaaaaaabbbababaaaaabbaabbbbbabbaaabab"
           ,"aabbaabbabbbaaaabbbabbaaabaabbbbababbaba"
           ,"abaabbbaaaabbbaabbaaaabbbaabbbbb"
           ,"bbbbabbbaaabaabbaaaaabba"
           ,"babbabaabaaababaabaabbabbbaaaaaa"
           ,"abbbbaababbbaabbabaaabbbbaaaaababaaabababbbbbaab"
           ,"bbaaaabbbbbbaaabbababaabaaabbbaababbbbabaaabbababbbababaaaaaabbbbbababba"
           ,"baaaabbbbbbbabbabbaababb"
           ,"bbaaabbaabbbaaaabaabbaba"
           ,"abaaaaabbbbbaabaaababbbbbbbaaaab"
           ,"aaaabbbaababaaabbbbbabbabbaaaabbaababababbbbbababaabbababbbbaaaabbbaaabb"
           ,"abaaabbbbbbaaaaabaabbbaa"
           ,"aabaababbbabbbbaaaabbbaabbaaaaabaaabaabb"
           ,"bababaabbababaabbbaaabaa"
           ,"baaaaaabbabaabbbbabaaabbbbabaabb"
           ,"bbabbbbbabbabbababbaaabbbabbababbaabbbbbbbbabbaa"
           ,"aabbbbbaabbbabbabababbbaaabbbbbaaaabaaab"
           ,"bbbabaabaabbbabababbabaababbbbbababbaaaa"
           ,"aaaabababaaababbbbbbbababbaabbbbbbbbaabbaababbbbaaababaaabbbaababaabbabbbbbbbbbb"
           ,"aaaabaaabbabbabaabbbaaab"
           ,"aaaabaabbbbaaaabbaaabaabbababababbbaabaaababbabaaabbabaa"
           ,"bbbabaaabaaaabbaabbbbaab"
           ,"abbabbbbbbabaaaababbbaabbaaaaababaabbabbbbbaababbabaaaaa"
           ,"abbabbaaaaaabababaaabbabaaaaabaabbbaabbaababaaba"
           ,"aababbbabaababaaabbbaabbbbbbabaaaabbaababababbaaaaaaababbbbbbabbaababbbaaababbaabbbbabab"
           ,"abbaaaabbbaaaaabbaabbaaaaabbabbbbbbabaabaabbabbbbbbabbbaabbababb"
           ,"baababbaababbabbbaabbaba"
           ,"bbababbaabbaababaaababbabbaaaaabbbabbabaabbbbababbabbbabababaaaa"
           ,"aabbbbababababbaabaabbaababbbbba"
           ,"abaaaabbaaabbaaabbabbbaa"
           ,"abbaaabbabaabbbabababaabbbbaabba"
           ,"ababaaababaaabbbabaaabaa"
           ,"aabbaabaaabbbbaabaabaaab"
           ,"aabaabbbabbaaaaaabbbbabaaabaabbaaaababbbaabbabbabbbbaaaaabbaabaa"
           ,"baaabaabbaaaaaaabaaaaaabbababaaaabbbababbbbbaaaa"
           ,"aababbbabbabaaabbababaaaabaabaaaabbbaaba"
           ,"abbabbbabbbabaaabbabababbbabaaaabbaaaaaaaaabababbbabbbab"
           ,"bbbabaaaaaaabbabababbaabaaaaabaaabaabbbabbbabbbabaababbbbbababbbaaaaabbb"
           ,"baabababaaaaabbbbbbabbbb"
           ,"abbaaabbaaabaabaabaabaab"
           ,"abbbbbabbabaaababbabaabbbbabaaaaabbbbaabbaaabababaabaaaabababbbabbabbbaaabbaaabbababbabb"
           ,"ababaaababaabbbabbbbaabb"
           ,"bbaababaabababbaabbbabbbbbabbabaabaabbaa"
           ,"babaabbbababbbbabaaaababbbaaaabababababaabababab"
           ,"bbababbabbbbababaabbabaa"
           ,"bbaaaabababaababaaaabbbaabaaaaabbaabbbabbbbaabba"
           ,"bbabaababbbabaabaabaababababbbaabbbbbaabbbabbaba"
           ,"bbababbabbabaaabaaaabbbb"
           ,"baababbbbbaabbaaabaabaaa"
           ,"abaaaaaaabbaaaababaabaabbbbbaabbbaaababb"
           ,"abaaaabbbabbabababbbbaaa"
           ,"abbbababaabbaabbbaaabbaabbbabbbbaabbbbbabbbbaabb"
           ,"aabbaabbbbabbaaaaabbbbba"
           ,"bbbbabbaabaaaabbbaaababbabbababa"
           ,"babbbabaaabaaaaaabbbbbabaabbabab"
           ,"baaaaaababbbababbbbababa"
           ,"baabbaabaabaaaaabaaabaabaaaabaabaabbbbba"
           ,"aabaabbbbabbababaaabaabaaabaabaa"
           ,"aabbaabbbabbabbbbababaababaaaaabbbaababaabbabbaabbabaaabbbabbaab"
           ,"bbabbaabbabaaabaabbbaabbbbbaaaaaaaaabbaaaabaabaa"
           ,"babbaaabbabaabbbaababbba"
           ,"aaaaabbbbbaaabbaabbabbaabababbbbaaababba"
           ,"babbbaaaabaaabbabaaabbabbbabbaabbbababbb"
           ,"babbbabbbabbbbaabababbaaabababbbbbaababbbbaabaaabbbbaabb"
           ,"bbabbbbbaabbbabbbabaabbbabababaa"
           ,"abbbabbabaaabbbbaaaabaaa"
           ,"babbbbaabaaaaaababaabbbb"
           ,"aaaababababbbaabbbbaabbbbabbaaabbbaaaaaa"
           ,"abbbaabbbababaaabaaaaabb"
           ,"bbaabbaabbaaabbabbaaabbaabaabaabbaaabbbb"
           ,"bababbababbabbbbabababbaaaabaabaaabaaababbaabaabbbabbabbaabbabbbaaaaaaababaaabba"
           ,"abbababaaaabaaabbaabbaba"
           ,"baaababbbabaaabaaaaabaaaabaaaaababbbbbbbbabbabbbabbbabaabaababbabaaabbaaaaabaaabaabaaabababbbaab"
           ,"bbaaabbbabbabbaaabababab"
           ,"aabababaabbabbbbbbababbaaaaabbbbabbbbbba"
           ,"aaaaaabbaaabababaababaaabaaababb"
           ,"abbabaabbbbaaaaaabbaaaba"
           ,"abbbababaaabaababaaaaaaababaaaababababbbaabbbbba"
           ,"abbabbaababbbbababbaaaabbbbaabbbbabbbabbbabbabba"
           ,"bbbaabaaaaabbbaababbaabbbbbabaabbaaababaaaaababb"
           ,"bbaaaaababaabaaabaaabbaaabbaababbbbaaaabbbbbbaba"
           ,"bbaaaabbabbbbbbbaaaaaabbabbbbbabaababbaabaaabaabbbabaaabbaaabbbabbbabaabaaaabbaaaabaaabb"
           ,"abbabbaabbaaabbbbabbbbaabbaabaababaabaaaabbbaaababaabaabaaabaaab"
           ,"abbbabaaaabbaaabbbbabbaa"
           ,"baabaaaabaaaabbaabaaaaab"
           ,"aabbbabaabaaabbbbabbabba"
           ,"bbababbabbbaabbbbbbaaaabbaaabbabbbaabaaa"
           ,"aaaabbbbbaaaababaabababababaabba"
  ,"aabbbabababbbaaaaaabbbbaaaabaaaa"
    ,"abbaaaabbababbbaabbabbabababaababbbaaabb"
      ,"baaaaaabbbabaaaaabbaabbb"
      ,"aaaaaaabbbaaaabaabaaabaa"
      ,"aabbbbbbbbabbaabababaaba"
      ,"bbbbbbaaaaabbaaaabbaabbb"
      ,"bbaaabbbbabaaabbbaaaabaaaabbaabababbbabbabaaaabaabababbb"
      ,"abbbbbabaabbbabbbbbababb"
      ,"abaababaabbbaabbabbbaababbababbbbbbbbbba"
      ,"aabbbabaaaabbaaabbaabbaaabbbbaaa"
      ,"aabbabbbaaaabbabababbaba"
      ,"aaabbbbababbbaaababaabbbbaabbaba"
      ,"aabbbababbabbaaaabbaaaaaaaaaaaaabbbaabbaaabababb"
      ,"ababbbabbabaaababbaabbbb"
      ,"aabbbabbbaabbbabababbabbabbbabbabaaabbbabaabbbbb"
      ,"abbbabbaaabbbababbabbbbbbbbbabaabaaaaabbabbbbababbbababb"
      ,"baabaababaaaababbababbbb"
      ,"bbaaaaaabaabbbaaaaaabbbaaabbbbabbbaaabbbbbbbbbbbabbbbaaababaabaaaaaaaaabbbabaababaabaabb"
      ,"aababbbbabaaababbbabbaaaaaaaaabbaabbbbbaaababababababaababbaabababababbbabbbababbbaabaab"
      ,"bbabaabaaabbbbbbbababbaa"
      ,"aaababbabbbbababaababaabbbababaabbbabaababaaaabbaabbbaaa"
      ,"bbababbaaabbbaaaabbaabbb"
      ,"aabbbabaababbbaabbbabaaaaaaabbbabbbbbbaaabbababaabbaaabaabaabbaa"
      ,"ababbbaaaabaabbbbbbbbbbb"
      ,"abbbabaababbabaababaabba"
      ,"aaabbbaababbabaabbbbaababbabbbab"
      ,"bbaaaababaaabbbabbaababbababbababaaabaaa"
      ,"bbaaabbaabbaababaaaaabbbbaabbbaaabbbbaaa"
      ,"aaaaabababbbaaaabbbbbbabbabbbaababbabbaabbbbaabb"
      ,"aaabbaaaabbbabaabbbbbabbbbaababbbbbbbbbabbaaaabaabbbabbbaaaabbabbbabbaaaaaaaabbbbbabaaba"
      ,"babbbaabbaabaabaaaaabbbabbbbaabaaaabaabbbbabaabb"
      ,"abbabbabbabaaabaabbaabaa"
      ,"abaaaabbaaaaaaabbaaabbbabbaaabaa"
      ,"bbbbbbaababbababababbaba"
      ,"bbaaabbaabbbabbbabbaabba"
      ,"babaabbbaabaababaabaabba"
      ,"ababbbabbabbbaaabababbab"
      ,"bbaabbaaaaaaaaabbaabaaaa"
      ,"abbbbbabbbbabaaaaaabbbbabbbbbbbabaabbbaaaababbab"
      ,"bbbbaababaaabbabababaaba"
      ,"babbabaababbaaabaababbaa"
      ,"babbbaaaababaaababbbabaaabbbabaaabbbbbba"
      ,"bbbaabaababbababbbabaabb"
      ,"aababbbbabbabbbbababbbabbbabbababbababab"
      ,"baaaabbbbaaabbabbaabaabaaababbaaaaaaaabbbaabbbaaaabaaabb"
      ,"aaaaaaaaababbbbbbbbaaaaabababaaabbbbbaaa"
      ,"aaaaabababbabaabbabbabaaaaaabaab"
      ,"ababbbabbabbbaabbaaababb"
      ,"abaaabbababaaaababbabbaaabaaabaa"
      ,"babbabbbbabaaabbaabbbbaaabbbabaababbbbbbabaabbabaaabbbba"
      ,"aaaaabbbabbabbbbbbaaaaaa"
      ,"aaababbabbabaaabbabbabbb"
      ,"babaabbbabbbabbbaabbbbbbbaabbaabaababbab"
      ,"aaabbaabbaabbbbaaaaabbabbbaaaabbbaabababaaabbbabbababaaabbaababb"
      ,"babbbbabababbabbbababbab"
      ,"bababbbbbaabaaabaabaaababbaabaabaaaaaaabbbababaaaaabaaba"
      ,"aaaaaaaabaaaabbbbbaaabaa"
      ,"bbabbbbbaabaaaaaaaaaabaaabaabababbbbbbaaaaababbaaaababbb"
      ,"aaabababaababbababbbbbaabbaabbba"
      ,"baababababaaabbbbaaabbbb"
      ,"ababbbabbaababbbbaaaaaababbbaabbaaaaabaabaabaaaa"
      ,"babbaabbaabbaabbbaaaabbbababbbbbbabbbabbaaaababbbaaabaaaaababbaaaaababbbbbbbabbaaaababaa"
      ,"aaaaaabbbbabaababbbaaababbababaa"
      ,"babbbbaababbaabbabbbbaba"
      ,"bbaabbaaaaaabbbbabababbbbbabbbbbbaaabaaaabbabbbabbabbaaaaaaababa"
      ,"ababbbaababbbaaaaabaabaa"
      ,"baaaabbbbbbbabbaabbabaaa"]

valid :: [(Char,String)] -> Bool
valid [] = False
valid rs = any (null . snd) rs

parseAll = map (readP_to_S rule0) messages

solution = filter valid parseAll
