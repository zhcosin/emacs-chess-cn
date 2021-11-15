;;
;; chess-cn.el
;;
;;
;;       emacs 中的字符界面中国象棋
;;
;;
;; 代码仓库: https://gitee.com/zhcosin/emacs-chess-cn
;;
;;
;; 字符棋盘如下:
;;
;; "車----馬----象----士----將----士----象----馬----車
;; |     |     |     | \\   |   / |     |     |     | 
;; |     |     |     |   \\ | /   |     |     |     | 
;; +-----+-----+-----+-----+-----+-----+-----+-----+ 
;; |     |     |     |   / | \\   |     |     |     | 
;; |     |     |     | /   |   \\ |     |     |     | 
;; +-----炮----+-----+-----+-----+-----+-----炮----+ 
;; |     |     |     |     |     |     |     |     | 
;; |     |     |     |     |     |     |     |     | 
;; 卒----+-----卒----+-----卒----+-----卒----+-----卒
;; |     |     |     |     |     |     |     |     | 
;; |     |     |     |     |     |     |     |     | 
;; +-----+-----+-----+-----+-----+-----+-----+-----+ 
;; |                                               | 
;; |                                               | 
;; +-----+-----+-----+-----+-----+-----+-----+-----+ 
;; |     |     |     |     |     |     |     |     | 
;; |     |     |     |     |     |     |     |     | 
;; 兵----+-----兵----+-----兵----+-----兵----+-----兵
;; |     |     |     |     |     |     |     |     | 
;; |     |     |     |     |     |     |     |     | 
;; +-----炮----+-----+-----+-----+-----+-----炮----+ 
;; |     |     |     | \\   |   / |     |     |     | 
;; |     |     |     |   \\ | /   |     |     |     | 
;; +-----+-----+-----+-----+-----+-----+-----+-----+ 
;; |     |     |     |   / | \\   |     |     |     | 
;; |     |     |     | /   |   \\ |     |     |     | 
;; 車----馬----相----仕----帥----仕----相----馬----車"
;;
;; 使用方法:
;; 
;; 1. 在 emacs 中打开打开本文件，并使用 eval-buffer 命令读入文件内容.
;; 2. 命令 chess-new 可以开始新棋局，会打开一个名为 *chess-cn* 的缓冲区并绘制有初始棋局.
;; 3. 使用方向键移动光标到某一位置(或 hjkl 类 evil 操作)，按下回车键选择要走的棋子，再移动光标到目标位置，再按下回车键.
;;    (目标位置无棋子视为移动，有对方棋子为吃子，有己方棋子为重新选择要走的棋子)
;; 4. 红蓝双方交替走子，直至分出胜负.
;; 
;;

;; 启用 font-lock 时设置文本外观应使用 font-lock-face 文本属性，未启用 font-lock 时应使用 face 属性.

;; {{{ 辅助函数

;; elisp 中的 or 是一个 macro 而非 function，作代理
(defun chess-or-fun (x y)
  (or x y))

(defun scale-string (str num)
  "反复拼接同一字符串若干次"
  (cond
   ((= num 1)
      str)
   ((> num 1)
    (concat str (scale-string str (1- num))))
   (t nil)))

(defun chess-range-between-p (a b x &optional left-eq right-eq)
  "判断x是否在 a 与 b 之间(a与b大小无要求)，left-eq 与 right-eq 为是否允许等于左右边界"
  (and (funcall (if left-eq '>= '>) x (min a b)) (funcall (if right-eq '<= '<) x (max a b))))

(defun chess-get-range-between-sorted (a b)
  "得到 a 与 b(>a) 之间的整数列表"
  (if (>= (1+ a) b) nil (cons (1+ a) (chess-get-range-between-sorted (1+ a) b))))

(defun chess-get-range-between (a b)
  "得到 a 与 b (未指定大小)之间的整数列表"
  (chess-get-range-between-sorted (min a b) (max a b)))

;; 通用累加器
(defun chess-accumulate (li processor init-value accumulator)
  "累加器"
  ;;(message (format "accumulate for list %s with initial value %s by elemente processor %s and accumulator %s" li init-value processor accumulator))
  (if li
      (chess-accumulate (cdr li) processor (funcall accumulator init-value (funcall processor (car li))) accumulator)
    init-value))

(defun chess-coordinate-cycle (cord xinc yinc)
  "棋盘坐标增量计算，超出范围则取余循环"
  (cons (mod (+ xinc (car cord)) 9) (mod (+ yinc (cdr cord)) 10)))

;;; }}}


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




;; 兵种
(defconst chess-piece-type-ju '(name (side-blue "車" side-red "車") move-rule chess-move-rule-ju kill-rule chess-kill-rule-ju is-king nil) "")
(defconst chess-piece-type-ma '(name (side-blue "馬" side-red "馬") move-rule chess-move-rule-ma kill-rule chess-kill-rule-ma is-king nil) "")
(defconst chess-piece-type-pao '(name (side-blue "砲" side-red "炮") move-rule chess-move-rule-pao kill-rule chess-kill-rule-pao is-king nil) "")
(defconst chess-piece-type-bingzu '(name (side-blue "卒" side-red "兵") move-rule chess-move-rule-bingzu kill-rule chess-kill-rule-bingzu is-king nil) "")
(defconst chess-piece-type-xiang '(name (side-blue "象" side-red "相") move-rule chess-move-rule-xiang kill-rule chess-kill-rule-xiang is-king nil) "")
(defconst chess-piece-type-shi '(name (side-blue "士" side-red "仕") move-rule chess-move-rule-shi kill-rule chess-kill-rule-shi is-king nil) "")
(defconst chess-piece-type-jiangshuai '(name (side-blue "將" side-red "帥") move-rule chess-move-rule-jiangshuai kill-rule chess-kill-rule-jiangshuai is-king t) "")

;; 蓝方棋子
(defconst chess-piece-blue-jiang '(side side-blue type chess-piece-type-jiangshuai) "蓝将")
(defconst chess-piece-blue-xiang-1 '(side side-blue type chess-piece-type-xiang) "蓝象1")
(defconst chess-piece-blue-xiang-2 '(side side-blue type chess-piece-type-xiang) "蓝象2")
(defconst chess-piece-blue-shi-1 '(side side-blue type chess-piece-type-shi) "蓝士1")
(defconst chess-piece-blue-shi-2 '(side side-blue type chess-piece-type-shi) "蓝士2")
(defconst chess-piece-blue-ju-1 '(side side-blue type chess-piece-type-ju) "蓝车1")
(defconst chess-piece-blue-ju-2 '(side side-blue type chess-piece-type-ju) "蓝车2")
(defconst chess-piece-blue-ma-1 '(side side-blue type chess-piece-type-ma) "蓝马1")
(defconst chess-piece-blue-ma-2 '(side side-blue type chess-piece-type-ma) "蓝马2")
(defconst chess-piece-blue-pao-1 '(side side-blue type chess-piece-type-pao) "蓝炮1")
(defconst chess-piece-blue-pao-2 '(side side-blue type chess-piece-type-pao) "蓝炮2")
(defconst chess-piece-blue-zu-1 '(side side-blue type chess-piece-type-bingzu) "蓝卒1")
(defconst chess-piece-blue-zu-2 '(side side-blue type chess-piece-type-bingzu) "蓝卒2")
(defconst chess-piece-blue-zu-3 '(side side-blue type chess-piece-type-bingzu) "蓝卒3")
(defconst chess-piece-blue-zu-4 '(side side-blue type chess-piece-type-bingzu) "蓝卒4")
(defconst chess-piece-blue-zu-5 '(side side-blue type chess-piece-type-bingzu) "蓝卒5")

;; 红方棋子
(defconst chess-piece-red-shuai '(side side-red type chess-piece-type-jiangshuai) "红将")
(defconst chess-piece-red-xiang-1 '(side side-red type chess-piece-type-xiang) "红象1")
(defconst chess-piece-red-xiang-2 '(side side-red type chess-piece-type-xiang) "红象2")
(defconst chess-piece-red-shi-1 '(side side-red type chess-piece-type-shi) "红士1")
(defconst chess-piece-red-shi-2 '(side side-red type chess-piece-type-shi) "红士2")
(defconst chess-piece-red-ju-1 '(side side-red type chess-piece-type-ju) "红车1")
(defconst chess-piece-red-ju-2 '(side side-red type chess-piece-type-ju) "红车2")
(defconst chess-piece-red-ma-1 '(side side-red type chess-piece-type-ma) "红马1")
(defconst chess-piece-red-ma-2 '(side side-red type chess-piece-type-ma) "红马2")
(defconst chess-piece-red-pao-1 '(side side-red type chess-piece-type-pao) "红炮1")
(defconst chess-piece-red-pao-2 '(side side-red type chess-piece-type-pao) "红炮2")
(defconst chess-piece-red-bing-1 '(side side-red type chess-piece-type-bingzu) "红卒1")
(defconst chess-piece-red-bing-2 '(side side-red type chess-piece-type-bingzu) "红卒2")
(defconst chess-piece-red-bing-3 '(side side-red type chess-piece-type-bingzu) "红卒3")
(defconst chess-piece-red-bing-4 '(side side-red type chess-piece-type-bingzu) "红卒4")
(defconst chess-piece-red-bing-5 '(side side-red type chess-piece-type-bingzu) "红卒5")

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

(defvar chess-playing '(chess-game-over nil chess-curt-side nil chess-curt-selected-cord nil chess-situation nil)
  "对弈信息，包括对弈是否已结束、当前走子方、当前所选棋子的坐标、当前棋局(10x9二维棋子矩阵)")
(defun chess-playing-init ()
  "对弈信息初始化"
  (plist-put chess-playing 'chess-game-over nil)
  (plist-put chess-playing 'chess-situation (chess-copy-init-situation chess-init-situation)) ;; 初始棋局
  (plist-put chess-playing 'chess-curt-side nil)
  (plist-put chess-playing 'chess-curt-selected-cord nil))

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
  (insert "\n")
  (set-marker board-end (point)) ;; 棋盘结束位置标记
  (chess-playing-init) ;; 对弈信息初始化
  (draw-chess-board-by-situation (plist-get chess-playing 'chess-situation)) ;; 绘制棋盘
  (chess-move-point-to '(0 . 0))) ;; 初始化光标位置


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
             (unless (and (> (char-before) ?\x00) (< (char-before) ?\xff)) ;; 一个中文字符占据两个英文字符的位置
               (setq col (1+ col)))
             (when (char-equal (char-before) ?\n)
               (forward-char grid-offset)
               (setq row (1+ row))
               (setq col 0)))
           (cons (/ col grid-width) (/ row grid-high))))))

;; 棋盘坐标转换为缓冲区位置
(defun coordinate-to-position (cord)
  (and (>= (car cord) 0)
       (<= (car cord) 8)
       (>= (cdr cord) 0)
       (<= (cdr cord) 9)
       (let ((row 0) (col 0) (pos board-start))
         (let ((board-at-row (nth row (plist-get chess-playing 'chess-situation))))
           (while (< row (cdr cord))
             (setq board-at-row (nth row (plist-get chess-playing 'chess-situation)))
             (setq pos (+ pos grid-offset)) ;; 棋盘左侧偏移
             (while (< col 9)
               ;; 若 (col . row) 处有棋子，则增加 grid-width - 1 个位置，否则 增加 grid-width 个位置
               (setq pos (+ pos (if (null (nth col board-at-row)) (if (= col 8) 2 grid-width) (if (= col 8) 1 (1- grid-width)))))
               (setq col (1+ col)))  
             (setq pos (1+ pos))  ;; 换行符占据一个位置
             (setq pos (+ pos (* grid-offset (1- grid-high)))) ;; 棋盘左侧偏移(棋盘方格调试纯字符行)
             (setq pos (+ pos (* (1- grid-high) (+ 3 (* grid-width 8))))) ;; 棋盘方格高度产生的纯字符行，加上末尾的棋子位置(2个字符)和1个换行符.
             (setq row (1+ row))
             (setq col 0))
           (setq board-at-row (nth row (plist-get chess-playing 'chess-situation)))
           (setq pos (+ pos grid-offset)) ;; 棋盘左侧偏移
           (while (< col (car cord))
               ;; 若 (col . row) 处有棋子，则增加 grid-width - 1 个位置，否则 增加 grid-width 个位置
               (setq pos (+ pos (if (null (nth col board-at-row)) (if (= col 8) 2 grid-width) (if (= col 8) 1 (1- grid-width)))))
               (setq col (1+ col)))
           )
         pos)))

(define-derived-mode chinese-chess-mode special-mode "Chinese-Chess"
  "中国象棋主模式"
  (make-local-variable 'chess-playing)
  (define-key chinese-chess-mode-map (kbd "<up>") 'chess-move-point-up)
  (define-key chinese-chess-mode-map (kbd "<down>") 'chess-move-point-down)
  (define-key chinese-chess-mode-map (kbd "<left>") 'chess-move-point-left)
  (define-key chinese-chess-mode-map (kbd "<right>") 'chess-move-point-right))

(add-hook 'chinese-chess-mode-hook (lambda () (setq-local global-hl-line-mode nil)))


;;(add-to-list 'evil-emacs-state-modes 'chinese-chess-mode)
(evil-define-key 'normal chinese-chess-mode-map (kbd "RET") 'chess-step-cmd)

(evil-define-key 'normal chinese-chess-mode-map (kbd "<up>") 'chess-move-point-up)
(evil-define-key 'normal chinese-chess-mode-map (kbd "<down>") 'chess-move-point-down)
(evil-define-key 'normal chinese-chess-mode-map (kbd "<left>") 'chess-move-point-left)
(evil-define-key 'normal chinese-chess-mode-map (kbd "<right>") 'chess-move-point-right)

(evil-define-key 'normal chinese-chess-mode-map (kbd "k") 'chess-move-point-up)
(evil-define-key 'normal chinese-chess-mode-map (kbd "j") 'chess-move-point-down)
(evil-define-key 'normal chinese-chess-mode-map (kbd "h") 'chess-move-point-left)
(evil-define-key 'normal chinese-chess-mode-map (kbd "l") 'chess-move-point-right)


(defun chess-get-piece-from-situation (cord)
  "根据坐标获取棋局上的棋子"
  (when cord (nth (car cord) (nth (cdr cord) (plist-get chess-playing 'chess-situation)))))

(defun chess-set-piece-to-situation (cord piece)
  "根据坐标设置棋子"
  (setf (nth (car cord) (nth (cdr cord) (plist-get chess-playing 'chess-situation))) piece))

(defun chess-get-other-side (side)
  "获取对弈对方"
  (if (eq side 'side-blue) 'side-red 'side-blue))

(defun chess-select-piece (cord)
  "选择棋子, 更新当前所选棋子，并将允许走子方设为所选棋子所属方(以应对棋局首步棋)"
  (plist-put chess-playing 'chess-curt-selected-cord cord)
  (plist-put chess-playing 'chess-curt-side (plist-get (chess-get-piece-from-situation cord) 'side))
  (chess-step-debug))

;; 走子
;; 走子之前先判断是否符合兵种走棋规则，包括基本规则及棋局规则
(defun chess-move-piece (oldcord dstcord)
  "移动棋子"
  ;;(message (format "move piece from %s to %s" oldcord dstcord))
  (if
      (and 
       (chess-move-kill-base-rule oldcord dstcord)
       (funcall
        (plist-get (symbol-value (plist-get (chess-get-piece-from-situation oldcord) 'type)) 'move-rule)
        oldcord
        dstcord
        (plist-get chess-playing 'chess-situation)))
      (progn
          (chess-set-piece-to-situation dstcord (chess-get-piece-from-situation oldcord))
          (chess-set-piece-to-situation oldcord nil)
          (plist-put chess-playing 'chess-curt-selected-cord nil)
          (plist-put chess-playing 'chess-curt-side (chess-get-other-side (plist-get chess-playing 'chess-curt-side))))
    (message "违反走子规则")))

(defun chess-kill-piece (oldcord dstcord)
  "吃子"
  ;;(message (format "kill piece %s by %s" dstcord oldcord))
  (if
      (and 
       (chess-move-kill-base-rule oldcord dstcord)
       (funcall
        (plist-get (symbol-value (plist-get (chess-get-piece-from-situation oldcord) 'type)) 'kill-rule)
        oldcord
        dstcord
        (plist-get chess-playing 'chess-situation)))
      (progn
        (let ((killed-piece (chess-get-piece-from-situation dstcord))
              (kill-piece (chess-get-piece-from-situation oldcord)))
          (message (format "%s 被吃掉." killed-piece))
          (chess-set-piece-to-situation dstcord kill-piece)
          (chess-set-piece-to-situation oldcord nil)
          (plist-put chess-playing 'chess-curt-selected-cord nil)
          (plist-put chess-playing 'chess-curt-side (chess-get-other-side (plist-get chess-playing 'chess-curt-side)))
          (when (plist-get (symbol-value (plist-get killed-piece 'type)) 'is-king) ;; 被吃掉的棋子是将帅，游戏结束
              (progn
                (plist-put chess-playing 'chess-game-over t) 
                (message (format "游戏结束, %s胜出." (plist-get (symbol-value (plist-get kill-piece 'side)) 'name)))))))
    (message "违反吃子规则")))

(defun chess-allow-side-p (side)
  "是否为允许走子方"
  (or (null (plist-get chess-playing 'chess-curt-side)) (eq (plist-get chess-playing 'chess-curt-side) side)))

(defun chess-step-cmd ()
  "走子棋步命令"
  (interactive)
  (if (plist-get chess-playing 'chess-game-over)
      (message "对弈已结束")
    (chess-step)))

(defun chess-step ()
  "走子棋步"
  (let ((cord (position-to-coordinate (point))))
        ;;(message (format "落子位置 %s, 落子处棋子 %s，当前选子位置 %s，当前选子 %s." cord (chess-get-piece-from-situation cord) chess-curt-selected-cord (chess-get-piece-from-situation chess-curt-selected-cord)))
    (if cord
        (let ((piece-at-point (chess-get-piece-from-situation cord)))
          (if (plist-get chess-playing 'chess-curt-selected-cord)  ;; 当前选子非空
              (if piece-at-point ;; 光标处有棋子
                  (if (chess-allow-side-p (plist-get piece-at-point 'side)) (chess-select-piece cord) (chess-kill-piece (plist-get chess-playing 'chess-curt-selected-cord) cord))
                (chess-move-piece (plist-get chess-playing 'chess-curt-selected-cord) cord))
            (if piece-at-point
                (if (chess-allow-side-p (plist-get piece-at-point 'side)) (chess-select-piece cord) (message "无效棋步,当前应对方走子."))
              (message "无效棋步，当前未选择棋子且目标位置处无棋子."))
            ))
      (message "落子位置无效"))
    (draw-chess-board-by-situation (plist-get chess-playing 'chess-situation)) ;; 重新绘制棋盘
    ;;(message (format "move point to %s" (coordinate-to-position cord)))
    (goto-char (coordinate-to-position cord)) ;; 移动光标到落子位置(有coordinate-to-position 有bug)
    )) 
  
(defun chess-step-debug ()
  "棋步调试"
  (interactive)
  (message (format "当前走子方: %s, 当前选子: %s" (plist-get chess-playing 'chess-curt-side) (plist-get chess-playing 'chess-curt-selected-cord))))

;; {{{ rule

(defun chess-move-rule-stup-always-allow (oldcord dstcord situation)
  "移动规则桩，测试使用"
  t)

(defun chess-kill-rule-stup-always-allow (oldcord dstcord situation)
  "吃子规则桩，测试使用"
  t)

(defun chess-move-kill-base-rule (oldcord dstcord)
  "走子/吃子基本规则，不能走出棋盘范围外，不能原地踏步"
  (and (and (>= (car cord) 0) (< (car cord) 9))
       (and (>= (cdr cord) 0) (< (cdr cord) 10))
       (not (equal oldcord dstcord))))


(defun chess-move-line-rule (oldcord dstcord)
  "直线判断"
  (or (equal (car oldcord) (car dstcord)) (equal (cdr oldcord) (cdr dstcord))))

(defun chess-move-rule-ju (oldcord dstcord situation)
  "车的移动规则，判断 oldcord 与 dstcord 之间(不含端口)上是否有棋子，有棋子则不能移动，否则可以移动"
  (and
   (chess-move-line-rule oldcord dstcord)
   (not
    (chess-accumulate
     (cond
      ((equal (car oldcord) (car dstcord))
       (mapcar (lambda (x) (cons (car oldcord) x))
                (chess-get-range-between (cdr oldcord) (cdr dstcord))))
      ((equal (cdr oldcord) (cdr dstcord))
       (mapcar (lambda (x) (cons x (cdr oldcord)))
               (chess-get-range-between (car oldcord) (car dstcord))))
      nil)
     (lambda (cord) (chess-get-piece-from-situation cord))
     nil 
     'chess-or-fun))))

(defun chess-kill-rule-ju (oldcord dstcord situation)
  "车的吃子规则，与移动规则相同"
  (chess-move-rule-ju oldcord dstcord situation))

(defun chess-move-rule-ma (oldcord dstcord situation)
  "马的移动规则"
  (cond
   ((and (equal 1 (abs (- (car oldcord) (car dstcord)))) (equal 2 (abs (- (cdr oldcord) (cdr dstcord))))) ;; 竖日字
    (not (chess-get-piece-from-situation (cons (car oldcord) (/ (+ (cdr oldcord) (cdr dstcord)) 2))))) ;; 绊腿判断
   ((and (equal 2 (abs (- (car oldcord) (car dstcord)))) (equal 1 (abs (- (cdr oldcord) (cdr dstcord))))) ;; 横日字
    (not (chess-get-piece-from-situation (cons (/ (+ (car oldcord) (car dstcord)) 2) (cdr oldcord))))) ;; 绊腿判断
   nil))

(defun chess-kill-rule-ma (oldcord dstcord situation)
  "马的吃子规则"
  (chess-move-rule-ma oldcord dstcord situation))

(defun chess-move-rule-pao (oldcord dstcord situation)
  "炮的移动规则，与车相同"
  (chess-move-rule-ju oldcord dstcord situation))

(defun chess-kill-rule-pao (oldcord dstcord situation)
  "炮的吃子规则，需要有炮台"
  (and
   (chess-move-line-rule oldcord dstcord)
   (equal
    1
    (chess-accumulate
     (cond
      ((equal (car oldcord) (car dstcord))
       (mapcar (lambda (x) (cons (car oldcord) x))
                (chess-get-range-between (cdr oldcord) (cdr dstcord))))
      ((equal (cdr oldcord) (cdr dstcord))
       (mapcar (lambda (x) (cons x (cdr oldcord)))
               (chess-get-range-between (car oldcord) (car dstcord))))
      0)
     (lambda (cord) (if (chess-get-piece-from-situation cord) 1 0))
     0 
     '+))))

(defun chess-move-rule-jiangshuai (oldcord dstcord situation)
  "将帅走子规则"
  (and
   (chess-range-between-p 3 5 (car dstcord) t t) ;; 不能离开九宫格
   (if
     (eq 'side-red (plist-get (chess-get-piece-from-situation oldcord) 'side))
     (chess-range-between-p 7 9 (cdr dstcord) t t)
     (chess-range-between-p 0 2 (cdr dstcord) t t))
   (equal 1 (+ (abs (- (car oldcord) (car dstcord))) (abs (- (cdr oldcord) (cdr dstcord))))))) ;; 只能单步走
;; TODO: 将帅不可见面

(defun chess-kill-rule-jiangshuai (oldcord dstcord situation)
  "将帅吃子规则，与走子规则相同"
  (chess-move-rule-jiangshuai oldcord dstcord situation))

(defun chess-move-rule-shi (oldcord dstcord situation)
  "士仕走子规则"
  (and
   (chess-range-between-p 3 5 (car dstcord) t t) ;; 不能离开九宫格
   (if
     (eq 'side-red (plist-get (chess-get-piece-from-situation oldcord) 'side))
     (chess-range-between-p 7 9 (cdr dstcord) t t)
     (chess-range-between-p 0 2 (cdr dstcord) t t))
   (equal 1 (abs (- (car oldcord) (car dstcord))))
   (equal 1 (abs (- (cdr oldcord) (cdr dstcord))))))

(defun chess-kill-rule-shi (oldcord dstcord situation)
  "士仕吃子规则"
  (chess-move-rule-shi oldcord dstcord situation))

(defun chess-move-rule-xiang (oldcord dstcord situation)
  "象相走子规则 "
  (and
   (and ;; 田字规则
    (equal 2 (abs (- (car oldcord) (car dstcord))))
    (equal 2 (abs (- (cdr oldcord) (cdr dstcord)))))
   (not (chess-get-piece-from-situation (cons (/ (+ (car oldcord) (car dstcord)) 2) (/ (+ (cdr oldcord) (cdr dstcord)) 2)))) ;; 未填心
   (if (eq 'side-red (plist-get (chess-get-piece-from-situation oldcord) 'side)) ;; 不可过河
       (chess-range-between-p 5 9 (cdr dstcord) t t)
     (chess-range-between-p 0 4 (cdr dstcord) t t))))

(defun chess-kill-rule-xiang (oldcord dstcord situation)
  "象相吃子规则"
  (chess-move-rule-xiang oldcord dstcord situation))

(defun chess-move-rule-bingzu (oldcord dstcord situation)
  "兵卒走子规则"
  (if
     (eq 'side-red (plist-get (chess-get-piece-from-situation oldcord) 'side))
      (or
          (and ;; 前进
           (equal (car oldcord) (car dstcord))
           (equal 1 (- (cdr oldcord) (cdr dstcord))))
          (and
           (< (cdr oldcord) 5) ;; 已过河
           (and (equal (cdr oldcord) (cdr dstcord)) (equal 1 (abs (- (car oldcord) (car dstcord)))))))
      (or
          (and ;; 前进
           (equal (car oldcord) (car dstcord))
           (equal -1 (- (cdr oldcord) (cdr dstcord))))
          (and
           (> (cdr oldcord) 4) ;; 已过河
           (and (equal (cdr oldcord) (cdr dstcord)) (equal 1 (abs (- (car oldcord) (car dstcord)))))))))

(defun chess-kill-rule-bingzu (oldcord dstcord situation)
  "兵卒吃子规则"
  (chess-move-rule-bingzu oldcord dstcord situation))

;; }}}

;; {{{ 光标移动命令
(defun chess-move-point-to (cord)
  "移动光标"
  (goto-char (coordinate-to-position cord)))

(defun chess-move-point-up ()
  "向上移动一格"
  (interactive)
  (chess-move-point-to (chess-coordinate-cycle (position-to-coordinate (point)) 0 -1)))

(defun chess-move-point-down ()
  "向下移动一格"
  (interactive)
  (chess-move-point-to (chess-coordinate-cycle (position-to-coordinate (point)) 0 1)))

(defun chess-move-point-left ()
  "向左移动一格"
  (interactive)
  (chess-move-point-to (chess-coordinate-cycle (position-to-coordinate (point)) -1 0)))

(defun chess-move-point-right ()
  "向右移动一格"
  (interactive)
  (chess-move-point-to (chess-coordinate-cycle (position-to-coordinate (point)) 1 0)))

;; }}}

(provide 'chess-cn)

;; end here.

