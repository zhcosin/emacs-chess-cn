;; 需要禁用 font-lock-mode


(defvar chess-init "")
(defvar chess-init-blue "")
(defvar chess-init-red "")

(setq chess-init-blue 
"車----馬----象----士----將----士----象----馬----車
|     |     |     | \\   |   / |     |     |     |
|     |     |     |   \\ | /   |     |     |     |
+-----+-----+-----+-----+-----+-----+-----+-----+
|     |     |     |   / | \\   |     |     |     |
|     |     |     | /   |   \\ |     |     |     |
+-----炮----+-----+-----+-----+-----+-----炮----+
|     |     |     |     |     |     |     |     |
|     |     |     |     |     |     |     |     |
卒----+-----卒----+-----卒----+-----卒----+-----卒
|     |     |     |     |     |     |     |     |
|     |     |     |     |     |     |     |     |
+-----+-----+-----+-----+-----+-----+-----+-----+
|                                               |
")

(setq chess-init-red 
"|                                               | 
+-----+-----+-----+-----+-----+-----+-----+-----+
|     |     |     |     |     |     |     |     |
|     |     |     |     |     |     |     |     |
兵----+-----兵----+-----兵----+-----兵----+-----兵
|     |     |     |     |     |     |     |     |
|     |     |     |     |     |     |     |     |
+-----炮----+-----+-----+-----+-----+-----炮----+
|     |     |     | \\   |   / |     |     |     |
|     |     |     |   \\ | /   |     |     |     |
+-----+-----+-----+-----+-----+-----+-----+-----+
|     |     |     |   / | \\   |     |     |     |
|     |     |     | /   |   \\ |     |     |     |
車----馬----相----仕----帅----仕----相----馬----車")


(setq chess-init-blue (replace-regexp-in-string "卒" (propertize "卒" 'face '(:background "blue")) chess-init-blue))
(setq chess-init-blue (replace-regexp-in-string "炮" (propertize "炮" 'face '(:background "blue")) chess-init-blue))
(setq chess-init-blue (replace-regexp-in-string "車" (propertize "車" 'face '(:background "blue")) chess-init-blue))
(setq chess-init-blue (replace-regexp-in-string "馬" (propertize "馬" 'face '(:background "blue")) chess-init-blue))
(setq chess-init-blue (replace-regexp-in-string "象" (propertize "象" 'face '(:background "blue")) chess-init-blue))
(setq chess-init-blue (replace-regexp-in-string "士" (propertize "士" 'face '(:background "blue")) chess-init-blue))
(setq chess-init-blue (replace-regexp-in-string "將" (propertize "將" 'face '(:background "blue")) chess-init-blue))

(setq chess-init-red (replace-regexp-in-string "兵" (propertize "兵" 'face '(:background "red")) chess-init-red))
(setq chess-init-red (replace-regexp-in-string "炮" (propertize "炮" 'face '(:background "red")) chess-init-red))
(setq chess-init-red (replace-regexp-in-string "車" (propertize "車" 'face '(:background "red")) chess-init-red))
(setq chess-init-red (replace-regexp-in-string "馬" (propertize "馬" 'face '(:background "red")) chess-init-red))
(setq chess-init-red (replace-regexp-in-string "相" (propertize "相" 'face '(:background "red")) chess-init-red))
(setq chess-init-red (replace-regexp-in-string "仕" (propertize "仕" 'face '(:background "red")) chess-init-red))
(setq chess-init-red (replace-regexp-in-string "帅" (propertize "帅" 'face '(:background "red")) chess-init-red))

(setq chess-init (concat chess-init-blue chess-init-red))

(insert chess-init)

