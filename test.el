;; 启用 font-lock 时设置文本外观应使用 font-lock-face 文本属性，未启用 font-lock 时应使用 face 属性.

;; 缓冲区名称
(defconst chess-buffer-name "*chess*")

;; 棋盘起止位置标记
(setq board-start (make-marker)) 
(setq board-end (make-marker)) 

;; 对战双方
(defconst side-blue '(name "蓝方" style (:background "blue")))
(defconst side-red '(name "红方" style (:background "red")))

(defconst regexp-cn "[^\x00-\xff]" "中文字符正则串")

(defconst grid-width 6 "棋盘小方格宽度字符数")
(defconst grid-high 3 "棋盘小方格高度字符数")


(defvar chess-situation nil "棋局,10x9二维矩阵，元素为棋子")
(defvar chess-init-situation
  '(
   (1 1 1 1 1 1 1 1 1)
   (nil nil nil nil nil nil nil nil nil)
   (nil 1 nil nil nil nil nil 1 nil)
   (1 nil 1 nil 1 nil 1 nil 1)
   (nil nil nil nil nil nil nil nil nil)
   (nil nil nil nil nil nil nil nil nil)
   (1 nil 1 nil 1 nil 1 nil 1)
   (nil 1 nil nil nil nil nil 1 nil)
   (nil nil nil nil nil nil nil nil nil)
   (1 1 1 1 1 1 1 1 1)
   )
  "初始棋局")
(setq chess-situation chess-init-situation)


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

(defconst chess-banner "\n\n        中国象棋        \n\n" "banner")

(defun chess-new ()
  (interactive)
  (get-buffer-create chess-buffer-name)
  (switch-to-buffer chess-buffer-name)
  (erase-buffer)
  (font-lock-mode 1)
  (insert chess-banner)
  (set-marker board-start (point)) ;; 棋盘开始位置标记
  (insert chess-init)
  (set-marker board-end (point)) ;; 棋盘结束位置标记
  (insert "\n")
  (setq chess-situation chess-init-situation)
  ;;(message (format "board start at %d and end at %d" (marker-position board-start) (marker-position board-end)))
  ;;(princ-list (get-board-pos 0))
  ;;(princ board-end)
  ;;(princ (cons board-start board-end))
  ;;(princ (position-to-coordinate 148))
  ;;(princ (position-to-coordinate (coordinate-to-position '(4 5))))
  (princ (coordinate-to-position (position-to-coordinate 1051)))
  )


;; 缓冲区位置转换为棋盘坐标
(defun position-to-coordinate (pos)
  (and (>= pos board-start)
       (< pos board-end)
       (save-excursion
         (let ((row 0)
               (col 0))
           (goto-char board-start)
           (while (< (point) pos)
             (forward-char)
             (setq col (1+ col))
             (unless (and (> (char-after) ?\x00) (< (char-after) ?\xff)) ;; 一个中文字符占据两个英文字符的位置
               (setq col (1+ col)))
             (when (char-equal (char-before) ?\n)
               (setq row (1+ row))
               (setq col 0)))
           (list (/ col grid-width) (/ row grid-high))))))

;; 棋盘坐标转换为缓冲区位置
(defun coordinate-to-position (cord)
  (and (>= (nth 0 cord) 0)
       (<= (nth 0 cord) 8)
       (>= (nth 1 cord) 0)
       (<= (nth 1 cord) 9)
       (let ((row 0) (col 0) (pos board-start))
         (let ((board-at-row (nth row chess-situation)))
           (while (< row (nth 1 cord))
             (setq board-at-row (nth row chess-situation))
             (while (< col 9)
               ;; 若 (col . row) 处有棋子，则增加 grid-width - 1 个位置，否则 增加 grid-width 个位置
               (setq pos (+ pos (if (null (nth col board-at-row)) (if (= col 8) 2 grid-width) (if (= col 8) 1 (1- grid-width)))))
               (setq col (1+ col)))  
             (setq pos (1+ pos))  ;; 换行符占据一个位置
             (setq pos (+ pos (* (1- grid-high) (+ 3 (* grid-width 8))))) ;; 棋盘方格高度产生的纯字符行，加上末尾的棋子位置(2个字符)和1个换行符.
             (setq row (1+ row))
             (setq col 0))
           (setq board-at-row (nth row chess-situation))
           (while (< col (nth 0 cord))
               ;; 若 (col . row) 处有棋子，则增加 grid-width - 1 个位置，否则 增加 grid-width 个位置
               (setq pos (+ pos (if (null (nth col board-at-row)) (if (= col 8) 2 grid-width) (if (= col 8) 1 (1- grid-width)))))
               (setq col (1+ col)))
           )
         pos)))

(coordinate-to-position '(8 9))
(position-to-coordinate 1395)
(nth 0 chess-situation)
(nth 7 (nth 0 chess-situation))

(chess-new)

(setq chess-init-str-arr (split-string chess-init "\n"))
(setq chess-init-str-arr-len (mapcar 'length chess-init-str-arr))
(message chess-init-str-arr-len)
(length chess-init)



;;(insert chess-init)
