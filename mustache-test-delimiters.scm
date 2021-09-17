(test-mustache "Pair Behavior"
               '((text . "Hey!"))
               "{{=<% %>=}}(<%text%>)"
               "(Hey!)")

(test-mustache "Special Characters"
               '((text . "It worked!"))
               "({{=[ ]=}}[text])"
               "(It worked!)")

(test-mustache "Sections"
               '((section . #t)
                 (data . "I got interpolated."))
               "[\n{{#section}}\n  {{data}}\n  |data|\n{{/section}}\n\n{{= | | =}}\n|#section|\n  {{data}}\n  |data|\n|/section|\n]\n"
               "[\n  I got interpolated.\n  |data|\n\n  {{data}}\n  I got interpolated.\n]\n")
