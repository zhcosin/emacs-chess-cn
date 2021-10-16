
(setq abc "哈另外大家好")
(propertize "馬" 'font-lock-face '(:background "red"))
(setq abc (propertize "中国第一" 'font-lock-face '(:background "blue")))
(put-text-property 0 (length abc) 'font-lock-face '(:background "red") abc)
(put-text-property 0 (length abc) 'font-lock-face 'bold abc)
(remove-text-properties 0 (length abc) '('vs-side "") abc)
(add-text-properties 0 (length abc) '('font-lock-face '('background "blue") 'pointer 'hand) abc)
(get-text-property 0 'vs-side abc)
(get-text-property 0 'font-lock-face abc)
(get-text-property 0 'hello abc)
(set-text-properties 0 (length abc) nil abc)
(text-properties-at 0 abc)

(insert abc)
哈另外大家好
哈另外大家好
哈另外大家好
哈另外大家好
哈另外大家好
哈另外大家好
哈另外大家好
哈另外大家好
哈另外大家好
中国第一
abc

(put-text-property 0 (length abc) 'font-lock-face '(:foreground "blue") abc)
(put-text-property 0 (length abc) 'font-lock-face '(:background "red") abc)

(insert (propertize "马" 'face '(:background "red")))马
(insert (propertize "马" 'font-lock-face '(:background "red")))马
(insert (propertize "马" 'font-lock-face '(:background "blue")))马
abc
abc
abc
abc
abc
abc


(string-match "[^\x00-\xff]" "aaa車bbb")
(string-match "def" "abcdefg周12345")
(match-data)

(setq hello
      "車----馬----象----士----將----士----象----馬----車
|     |     |     | \   |   / |     |     |     |
|     |     |     |   \ | /   |     |     |     |
+-----+-----+-----+-----+-----+-----+-----+-----+
|     |     |     |   / | \   |     |     |     |
|     |     |     | /   |   \ |     |     |     |
+-----炮----+-----+-----+-----+-----+-----炮----+
|     |     |     |     |     |     |     |     |
|     |     |     |     |     |     |     |     |
卒----+-----卒----+-----卒----+-----卒----+-----卒
|     |     |     |     |     |     |     |     |
|     |     |     |     |     |     |     |     |
+-----+-----+-----+-----+-----+-----+-----+-----+
|                                               |
|                                               | 
+-----+-----+-----+-----+-----+-----+-----+-----+
|     |     |     |     |     |     |     |     |
|     |     |     |     |     |     |     |     |
兵----+-----兵----+-----兵----+-----兵----+-----兵
|     |     |     |     |     |     |     |     |
|     |     |     |     |     |     |     |     |
+-----炮----+-----+-----+-----+-----+-----炮----+
|     |     |     | \   |   / |     |     |     |
|     |     |     |   \ | /   |     |     |     |
+-----+-----+-----+-----+-----+-----+-----+-----+
|     |     |     |   / | \   |     |     |     |
|     |     |     | /   |   \ |     |     |     |
車----馬----相----仕----帅----仕----相----馬----車
"
      )



(string-match "[^\x00-\xff]" hello)
(match-data)
(length hello)

(let ((start 0))
  (while (string-match "[^\x00-\xff]" hello start)
    (princ (match-data))
    (setq start (match-end 0))))
