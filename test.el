;; 启用 font-lock 时设置文本外观应使用 font-lock-face 文本属性，未启用 font-lock 时应使用 face 属性.

(defun scale-string (str num)
  "反复拼接同一字符串若干次"
  (cond
   ((= num 1)
      str)
   ((> num 1)
    (concat str (scale-string str (1- num))))
   (t nil)))


;; 缓冲区名称
(defconst chess-buffer-name "*cn-chess*")

;; 棋盘起止位置标记
(defvar board-start (make-marker)) 
(defvar board-end (make-marker)) 

;; 对弈双方
(defconst side-blue '(name "蓝方" style (:background "blue")))
(defconst side-red '(name "红方" style (:background "red")))
(defun get-side-by-flag (flag)
  "根据对局方标识获取对局方信息"
  (symbol-value flag))


(defconst regexp-cn "[^\x00-\xff]" "中文字符正则串")

(defconst grid-width 6 "棋盘小方格宽度字符数")
(defconst grid-high 3 "棋盘小方格高度字符数")
(defconst grid-offset 8 "棋盘距左边界起始列号")



(defvar chess-init "")


(defconst chess-board 
"+-----+-----+-----+-----+-----+-----+-----+-----+ 
|     |     |     | \\   |   / |     |     |     | 
|     |     |     |   \\ | /   |     |     |     | 
+-----+-----+-----+-----+-----+-----+-----+-----+ 
|     |     |     |   / | \\   |     |     |     | 
|     |     |     | /   |   \\ |     |     |     | 
+-----+-----+-----+-----+-----+-----+-----+-----+ 
|     |     |     |     |     |     |     |     | 
|     |     |     |     |     |     |     |     | 
+-----+-----+-----+-----+-----+-----+-----+-----+ 
|     |     |     |     |     |     |     |     | 
|     |     |     |     |     |     |     |     | 
+-----+-----+-----+-----+-----+-----+-----+-----+ 
|                                               | 
|                                               | 
+-----+-----+-----+-----+-----+-----+-----+-----+ 
|     |     |     |     |     |     |     |     | 
|     |     |     |     |     |     |     |     | 
+-----+-----+-----+-----+-----+-----+-----+-----+ 
|     |     |     |     |     |     |     |     | 
|     |     |     |     |     |     |     |     | 
+-----+-----+-----+-----+-----+-----+-----+-----+ 
|     |     |     | \\   |   / |     |     |     | 
|     |     |     |   \\ | /   |     |     |     | 
+-----+-----+-----+-----+-----+-----+-----+-----+ 
|     |     |     |   / | \\   |     |     |     | 
|     |     |     | /   |   \\ |     |     |     | 
+-----+-----+-----+-----+-----+-----+-----+-----+ 
"
"棋盘")



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
車----馬----相----仕----帥----仕----相----馬----車")

(defconst chess-init-length (length chess-init))


;; 兵种
(defconst chess-piece-type-ju '(name (side-blue "車" side-red "車") move-rule nil kill-rule nil is-king nil) "")
(defconst chess-piece-type-ma '(name (side-blue "馬" side-red "馬") move-rule nil kill-rule nil is-king nil) "")
(defconst chess-piece-type-pao '(name (side-blue "砲" side-red "炮") move-rule nil kill-rule nil is-king nil) "")
(defconst chess-piece-type-bingzu '(name (side-blue "卒" side-red "兵") move-rule nil kill-rule nil is-king nil) "")
(defconst chess-piece-type-xiang '(name (side-blue "象" side-red "相") move-rule nil kill-rule nil is-king nil) "")
(defconst chess-piece-type-shi '(name (side-blue "士" side-red "仕") move-rule nil kill-rule nil is-king nil) "")
(defconst chess-piece-type-jiangshuai '(name (side-blue "將" side-red "帥") move-rule nil kill-rule nil is-king t) "")

;; 蓝方棋子
(defvar chess-piece-blue-jiang '(side side-blue type chess-piece-type-jiangshuai) "蓝将")
(defvar chess-piece-blue-xiang-1 '(side side-blue type chess-piece-type-xiang) "蓝象1")
(defvar chess-piece-blue-xiang-2 '(side side-blue type chess-piece-type-xiang) "蓝象2")
(defvar chess-piece-blue-shi-1 '(side side-blue type chess-piece-type-shi) "蓝士1")
(defvar chess-piece-blue-shi-2 '(side side-blue type chess-piece-type-shi) "蓝士2")
(defvar chess-piece-blue-ju-1 '(side side-blue type chess-piece-type-ju) "蓝车1")
(defvar chess-piece-blue-ju-2 '(side side-blue type chess-piece-type-ju) "蓝车2")
(defvar chess-piece-blue-ma-1 '(side side-blue type chess-piece-type-ma) "蓝马1")
(defvar chess-piece-blue-ma-2 '(side side-blue type chess-piece-type-ma) "蓝马2")
(defvar chess-piece-blue-pao-1 '(side side-blue type chess-piece-type-pao) "蓝炮1")
(defvar chess-piece-blue-pao-2 '(side side-blue type chess-piece-type-pao) "蓝炮2")
(defvar chess-piece-blue-zu-1 '(side side-blue type chess-piece-type-bingzu) "蓝卒1")
(defvar chess-piece-blue-zu-2 '(side side-blue type chess-piece-type-bingzu) "蓝卒2")
(defvar chess-piece-blue-zu-3 '(side side-blue type chess-piece-type-bingzu) "蓝卒3")
(defvar chess-piece-blue-zu-4 '(side side-blue type chess-piece-type-bingzu) "蓝卒4")
(defvar chess-piece-blue-zu-5 '(side side-blue type chess-piece-type-bingzu) "蓝卒5")

;; 红方棋子
(defvar chess-piece-red-shuai '(side side-red type chess-piece-type-jiangshuai) "红将")
(defvar chess-piece-red-xiang-1 '(side side-red type chess-piece-type-xiang) "红象1")
(defvar chess-piece-red-xiang-2 '(side side-red type chess-piece-type-xiang) "红象2")
(defvar chess-piece-red-shi-1 '(side side-red type chess-piece-type-shi) "红士1")
(defvar chess-piece-red-shi-2 '(side side-red type chess-piece-type-shi) "红士2")
(defvar chess-piece-red-ju-1 '(side side-red type chess-piece-type-ju) "红车1")
(defvar chess-piece-red-ju-2 '(side side-red type chess-piece-type-ju) "红车2")
(defvar chess-piece-red-ma-1 '(side side-red type chess-piece-type-ma) "红马1")
(defvar chess-piece-red-ma-2 '(side side-red type chess-piece-type-ma) "红马2")
(defvar chess-piece-red-pao-1 '(side side-red type chess-piece-type-pao) "红炮1")
(defvar chess-piece-red-pao-2 '(side side-red type chess-piece-type-pao) "红炮2")
(defvar chess-piece-red-bing-1 '(side side-red type chess-piece-type-bingzu) "红卒1")
(defvar chess-piece-red-bing-2 '(side side-red type chess-piece-type-bingzu) "红卒2")
(defvar chess-piece-red-bing-3 '(side side-red type chess-piece-type-bingzu) "红卒3")
(defvar chess-piece-red-bing-4 '(side side-red type chess-piece-type-bingzu) "红卒4")
(defvar chess-piece-red-bing-5 '(side side-red type chess-piece-type-bingzu) "红卒5")



(defvar chess-curt-selected-cord nil "当前所选择的棋子坐标")
(defvar chess-curt-side nil "当前走子对弈方")
;; 棋局
(defvar chess-situation nil "棋局,10x9二维矩阵，元素为棋子")
(defconst chess-init-situation
  (list 
   (list chess-piece-blue-ju-1 chess-piece-blue-ma-1 chess-piece-blue-xiang-1 chess-piece-blue-shi-1 chess-piece-blue-jiang chess-piece-blue-shi-2 chess-piece-blue-xiang-2 chess-piece-blue-ma-2 chess-piece-blue-ju-2)
   (list nil nil nil nil nil nil nil nil nil)
   (list nil chess-piece-blue-pao-1 nil nil nil nil nil chess-piece-blue-pao-2 nil)
   (list chess-piece-blue-zu-1 nil chess-piece-blue-zu-2 nil chess-piece-blue-zu-3 nil chess-piece-blue-zu-4 nil chess-piece-blue-zu-5)
   (list nil nil nil nil nil nil nil nil nil)
   (list nil nil nil nil nil nil nil nil nil)
   (list chess-piece-red-bing-1 nil chess-piece-red-bing-2 nil chess-piece-red-bing-3 nil chess-piece-red-bing-4 nil chess-piece-red-bing-5)
   (list nil chess-piece-red-pao-1 nil nil nil nil nil chess-piece-red-pao-2 nil)
   (list nil nil nil nil nil nil nil nil nil)
   (list chess-piece-red-ju-1 chess-piece-red-ma-1 chess-piece-red-xiang-1 chess-piece-red-shi-1 chess-piece-red-shuai chess-piece-red-shi-2 chess-piece-red-xiang-2 chess-piece-red-ma-2 chess-piece-red-ju-2)
   )
  "初始棋局")


(defun get-side-of-chess-piece (chess-piece)
  "获取棋子的对弈方信息"
  (get-side-by-flag (plist-get chess-piece 'side)))

(defun get-chess-piece-name (chess-piece)
  "获取棋子名称"
  (plist-get (plist-get (symbol-value (plist-get chess-piece 'type)) 'name) (plist-get chess-piece 'side)))



(defun get-chess-piece-face (chess-piece)
  "获取棋子用于显示的文本属性"
  (plist-get (symbol-value (plist-get chess-piece 'side)) 'style)
  )


(defun draw-chess-situation (the-situation)
  "绘制棋局"
  (setq buffer-read-only nil)
  (delete-region board-start (1- board-end))
  (goto-char board-start)
  (while the-situation
    (insert (make-string grid-offset ? ))
    (let ((row-situation (car the-situation)))
      (while row-situation
        (let* ((curt-piece (car row-situation))
              (with-piece (not (null curt-piece)))
              (is-tail (null (cdr row-situation)))
              )
          (insert
           (cond
            ((and with-piece (not is-tail)) ;; 当前位置有棋子且不在行尾
             ;;(message "有，no")
             (concat (propertize (get-chess-piece-name curt-piece) 'font-lock-face (get-chess-piece-face curt-piece))
                     (make-string (- grid-width 2) ?-)))
            ((and with-piece is-tail) ;; 当前位置有棋子且在行尾
             ;;(message "有，yes")
             (concat (propertize (get-chess-piece-name curt-piece) 'font-lock-face (get-chess-piece-face curt-piece)) "\n"))
            ((and (not with-piece) (not is-tail)) ;; 当前位置无棋子且不在行尾
             ;;(message "无，no")
             (concat "+" (make-string (1- grid-width) ?-)))
            ((and (not with-piece) is-tail) ;; 当前位置无棋子且在行尾
             ;;(message "无，yes")
             "+ \n"))))
        (setq row-situation (cdr row-situation)) ;; 切换下一个棋子
        ))
    (when (cdr the-situation) ;; 棋局还有下一行，则插入中间文本行
      (insert ;; 棋局换行
       (scale-string
        (concat
          (make-string grid-offset ? )
          (scale-string (concat "|" (make-string (1- grid-width) ? )) 8)
          "| \n")
        (1- grid-high))))
    (setq the-situation (cdr the-situation)))
  (setq buffer-read-only t)) ;; 切换棋局下一行

(defun put-chess-piece-to-board (row-situation board-row-str)
  "将棋局的一行输出到棋盘上的一行上"
  (let* ((board-row-str-len (length board-row-str))
             (max-width (if (< grid-width board-row-str-len) grid-width board-row-str-len)))
    (if row-situation
        (concat
         (if
           (car row-situation) ;; 有棋子
           (concat
             (propertize (get-chess-piece-name (car row-situation)) 'font-lock-face (get-chess-piece-face (car row-situation)))
             (substring board-row-str 2 max-width))
           (substring board-row-str 0 max-width))
         (put-chess-piece-to-board (cdr row-situation) (substring board-row-str max-width)))
      board-row-str)))

(defun draw-chess-board-by-situation (the-situation)
  "将棋局输出到棋盘"
  (setq buffer-read-only nil)
  (delete-region board-start (1- board-end))
  (goto-char board-start)
  (let ((i 0)
        (board-arr (split-string chess-board "\n")))
    (while (< i (length board-arr))
      (insert (concat (make-string grid-offset ? )
                      (if (= 0 (% i 3))
          (put-chess-piece-to-board (nth (/ i 3) the-situation) (nth i board-arr))
        (nth i board-arr)) "\n"))
      (setq i (1+ i))))
  (setq buffer-read-only t))


(defconst chess-banner "\n\n                             中国象棋        \n\n\n" "banner")

(defun chess-copy-init-situation (chess-init-situation)
  "深拷贝初始棋局"
  (if chess-init-situation
      (cons (copy-list (car chess-init-situation)) (chess-copy-init-situation (cdr chess-init-situation)))
    nil)
  )


(defun chess-new ()
  (interactive)
  (get-buffer-create chess-buffer-name)
  (switch-to-buffer chess-buffer-name)
  (chinese-chess-mode)
  (setq buffer-read-only nil)
  (erase-buffer)
  (font-lock-mode 1)
  (insert chess-banner)
  (set-marker board-start (point)) ;; 棋盘开始位置标记
  ;;(insert chess-init)
  (insert "\n")
  (set-marker board-end (point)) ;; 棋盘结束位置标记
  ;;(draw-chess-board chess-init-situation)
  (setq chess-situation (chess-copy-init-situation chess-init-situation)) ;; 初始棋局
  (setq chess-curt-side nil)
  (setq chess-curt-selected-cord nil)

  (draw-chess-board-by-situation chess-situation)
  ;;(setq buffer-read-only t)
  ;;(princ board-start)
  ;;(princ board-end)
  ;;(setq chess-situation chess-init-situation)
  ;;(message (format "board start at %d and end at %d" (marker-position board-start) (marker-position board-end)))
  ;;(princ-list (get-board-pos 0))
  ;;(princ board-end)
  ;;(princ (cons board-start board-end))
  ;;(princ (position-to-coordinate 148))
  ;;(princ (position-to-coordinate (coordinate-to-position '(4 5))))
  ;;(princ (coordinate-to-position (position-to-coordinate (+ 1051 board-start))))
  ;;(princ board-start)
  ;;(message "board start at %d, end at %d" (marker-position board-start) (marker-position board-end))
  ;;(delete-region board-start (1- board-end))
  ;;(goto-char board-start)
  ;;(insert "abcdefg")
  ;;(princ board-start)
  ;;(princ board-end)
  ;;(princ (position-to-coordinate 1655))
  )


;; 缓冲区位置转换为棋盘坐标
(defun position-to-coordinate (pos)
  (and (>= pos board-start)
       (< pos board-end)
       (save-excursion
         (let ((row 0)
               (col 0))
           (goto-char board-start)
           (forward-char grid-offset)
           (while (< (point) pos)
             (forward-char)
             (setq col (1+ col))
             (unless (and (> (char-after) ?\x00) (< (char-after) ?\xff)) ;; 一个中文字符占据两个英文字符的位置
               (setq col (1+ col)))
             (when (char-equal (char-before) ?\n)
               (forward-char grid-offset)
               (setq row (1+ row))
               (setq col 0)))
           (cons (/ col grid-width) (/ row grid-high))))))

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

(define-derived-mode chinese-chess-mode special-mode "Chinese-Chess"
  "中国象棋主模式"
  (make-local-variable 'chess-situation)
  (make-local-variable 'chess-curt-selected-cord)
  (make-local-variable 'chess-curt-side)
  ;;(setq-local glocal-hl-line-mode -1)
  ;;(define-key chinese-chess-mode-map (kbd "SPC")
    ;;(lambda () (interactive) (message "按下了空格键")))
  )

(add-hook 'chinese-chess-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

(defun chess-get-piece-from-situation (cord)
  "根据坐标获取棋局上的棋子"
  (nth (car cord) (nth (cdr cord) chess-situation)))

(defun chess-set-piece-to-situation (cord piece)
  "根据坐标设置棋子"
  (setf (nth (car cord) (nth (cdr cord) chess-situation)) piece))

(chess-get-piece-from-situation '(0 . 0))


(defun chess-get-other-side (side)
  "获取对弈对方"
  (if (eq side 'side-blue) 'side-red 'side-blue))

(defun chess-select-piece (cord)
  "选择棋子, 更新当前所选棋子，并将允许走子方设为所选棋子所属方(以应对棋局首步棋)"
  (setq chess-curt-selected-cord cord)
  (setq chess-curt-side (plist-get (chess-get-piece-from-situation cord) 'side))
  (chess-step-debug)
  )

(defun chess-move-piece (cord)
  "移动棋子"
  (chess-set-piece-to-situation cord (chess-get-piece-from-situation chess-curt-selected-cord))
  (chess-set-piece-to-situation chess-curt-selected-cord nil)
  (setq chess-curt-selected-cord nil)
  (setq chess-curt-side (chess-get-other-side chess-curt-side))
  (draw-chess-board-by-situation chess-situation) ;; 重新绘制棋盘
  (chess-step-debug)
  )

(defun chess-kill-piece (cord)
  "吃子"
  (chess-set-piece-to-situation cord (chess-get-piece-from-situation chess-curt-selected-cord))
  (chess-set-piece-to-situation chess-curt-selected-cord nil)
  (setq chess-curt-selected-cord nil)
  (setq chess-curt-side (chess-get-other-side chess-curt-side))
  (draw-chess-board-by-situation chess-situation) ;; 重新绘制棋盘
  (chess-step-debug)
  )

(defun chess-allow-side-p (side)
  "是否为允许走子方"
  (or (null chess-curt-side) (eq chess-curt-side side)))

(defun chess-step ()
  "走子棋步"
  (interactive)
  (let ((cord (position-to-coordinate (point))))
    (if cord
        ;;(message (format "落子位置 (%d, %d)" (car cord) (cdr cord)))
        (let ((piece-at-point (chess-get-piece-from-situation cord)))
          (if chess-curt-selected-cord  ;; 当前选子非空
              (if piece-at-point ;; 光标处有棋子
                  (if (chess-allow-side-p (plist-get piece-at-point 'side)) (chess-select-piece cord) (chess-kill-piece cord))
                (chess-move-piece cord))
            (if piece-at-point
                (if (chess-allow-side-p (plist-get piece-at-point 'side)) (chess-select-piece cord) (message "无效棋步,当前应对方走子."))
              (message "无效棋步，当前未选择棋子且目标位置处无棋子."))
            ))
      (message "落子位置无效"))
    )
  )

;;(add-to-list 'evil-emacs-state-modes 'chinese-chess-mode)
(evil-define-key 'normal chinese-chess-mode-map (kbd "RET") 'chess-step)
;;(add-hook 'chinese-chess-mode-hook (lambda () (progn (evil-mode -1) (global-hl-line-mode -1)))

(defun chess-step-debug ()
  "棋步调试"
  (interactive)
  (message (format "当前走子方: %s, 当前选子: %s" chess-curt-side chess-curt-selected-cord)))
