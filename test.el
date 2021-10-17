;; 启用 font-lock 时设置文本外观应使用 font-lock-face 文本属性，未启用 font-lock 时应使用 face 属性.

;; 对战双方
(defconst side-blue '(name "蓝方" style (:background "blue")))
(defconst side-red '(name "红方" style (:background "red")))

(defconst regexp-cn "[^\x00-\xff]" "中文字符正则串")

(defvar chess-init "")

(setq chess-init 
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
|                                               | 
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

(defconst chess-init-length (length chess-init))

(let ((start 0))
  (while (string-match regexp-cn chess-init start)  ;; 匹配中文
    ;;(princ (match-data))
    (put-text-property   ;; 添加队伍属性
      (match-beginning 0)
      (match-end 0)
      'font-lock-face
      (plist-get
        (if
          (< (match-beginning 0) (/ chess-init-length 2))
            side-blue
          side-red)
        'style)
      chess-init)
    (setq start (match-end 0))))

;;(get-text-property 0 'font-lock-face chess-init)



(insert chess-init)
