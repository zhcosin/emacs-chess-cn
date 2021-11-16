;;
;; chess-cn--cn.el
;;
;;
;;       emacs 中的字符界面中国象棋
;;
;;
;; 代码仓库: https://gitee.com/zhcosin/emacs-chess-cn--cn
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
;; 2. 命令 chess-cn--new 可以开始新棋局，会打开一个名为 *chess-cn--cn* 的缓冲区并绘制有初始棋局.
;; 3. 使用方向键移动光标到某一位置(或 hjkl 类 evil 操作)，按下回车键选择要走的棋子，再移动光标到目标位置，再按下回车键.
;;    (目标位置无棋子视为移动，有对方棋子为吃子，有己方棋子为重新选择要走的棋子)
;; 4. 红蓝双方交替走子，直至分出胜负.
;; 
;;

;; 启用 font-lock 时设置文本外观应使用 font-lock-face 文本属性，未启用 font-lock 时应使用 face 属性.

;; {{{ 辅助函数

;; elisp 中的 or 是一个 macro 而非 function，作代理
(defun chess-cn--or-fun (x y)
  (or x y))

(defun chess-cn--scale-string (str num)
  "反复拼接同一字符串若干次"
  (cond
   ((= num 1)
      str)
   ((> num 1)
    (concat str (chess-cn--scale-string str (1- num))))
   (t nil)))

(defun chess-cn--range-between-p (a b x &optional left-eq right-eq)
  "判断x是否在 a 与 b 之间(a与b大小无要求)，left-eq 与 right-eq 为是否允许等于左右边界"
  (and (funcall (if left-eq '>= '>) x (min a b)) (funcall (if right-eq '<= '<) x (max a b))))

(defun chess-cn--get-range-between-sorted (a b)
  "得到 a 与 b(>a) 之间的整数列表"
  (if (>= (1+ a) b) nil (cons (1+ a) (chess-cn--get-range-between-sorted (1+ a) b))))

(defun chess-cn--get-range-between (a b)
  "得到 a 与 b (未指定大小)之间的整数列表"
  (chess-cn--get-range-between-sorted (min a b) (max a b)))

;; 通用累加器
(defun chess-cn--accumulate (li processor init-value accumulator)
  "累加器"
  ;;(message (format "accumulate for list %s with initial value %s by elemente processor %s and accumulator %s" li init-value processor accumulator))
  (if li
      (chess-cn--accumulate (cdr li) processor (funcall accumulator init-value (funcall processor (car li))) accumulator)
    init-value))

(defun chess-cn--coordinate-cycle (cord xinc yinc)
  "棋盘坐标增量计算，超出范围则取余循环"
  (cons (mod (+ xinc (car cord)) 9) (mod (+ yinc (cdr cord)) 10)))

(defun chess-cn--get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))
;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman [zzbba…@aol.com]”. 2010-09-02

;;; }}}

;; {{{ 字符界面表示层

(defconst chess-cn--banner "\n\n                             中国象棋        \n\n\n" "banner")

(defconst chess-cn--board-grid-width 6 "棋盘小方格宽度字符数")
(defconst chess-cn--board-grid-high 3 "棋盘小方格高度字符数")
(defconst chess-cn--board-grid-offsetset 8 "棋盘距左边界起始列号")

;; 棋盘起止位置标记
(defvar chess-cn--board-start (make-marker)) 
(defvar chess-cn--board-end (make-marker)) 



(defconst chess-cn--board 
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

;; 缓冲区位置转换为棋盘坐标
(defun chess-cn--position-to-coordinate (pos)
  (and (>= pos chess-cn--board-start)
       (< pos chess-cn--board-end)
       (save-excursion
         (let ((row 0)
               (col 0))
           (goto-char chess-cn--board-start)
           (forward-char chess-cn--board-grid-offsetset)
           (while (< (point) pos)
             (forward-char)
             (setq col (1+ col))
             (unless (and (> (char-before) ?\x00) (< (char-before) ?\xff)) ;; 一个中文字符占据两个英文字符的位置
               (setq col (1+ col)))
             (when (char-equal (char-before) ?\n)
               (forward-char chess-cn--board-grid-offsetset)
               (setq row (1+ row))
               (setq col 0)))
           (cons (/ col chess-cn--board-grid-width) (/ row chess-cn--board-grid-high))))))

;; 棋盘坐标转换为缓冲区位置
(defun chess-cn--coordinate-to-position (cord)
  (and (>= (car cord) 0)
       (<= (car cord) 8)
       (>= (cdr cord) 0)
       (<= (cdr cord) 9)
       (let ((row 0) (col 0) (pos chess-cn--board-start))
         (let ((board-at-row (nth row (plist-get chess-cn--playing 'situation))))
           (while (< row (cdr cord))
             (setq board-at-row (nth row (plist-get chess-cn--playing 'situation)))
             (setq pos (+ pos chess-cn--board-grid-offsetset)) ;; 棋盘左侧偏移
             (while (< col 9)
               ;; 若 (col . row) 处有棋子，则增加 chess-cn--board-grid-width - 1 个位置，否则 增加 chess-cn--board-grid-width 个位置
               (setq pos (+ pos (if (null (nth col board-at-row)) (if (= col 8) 2 chess-cn--board-grid-width) (if (= col 8) 1 (1- chess-cn--board-grid-width)))))
               (setq col (1+ col)))  
             (setq pos (1+ pos))  ;; 换行符占据一个位置
             (setq pos (+ pos (* chess-cn--board-grid-offsetset (1- chess-cn--board-grid-high)))) ;; 棋盘左侧偏移(棋盘方格调试纯字符行)
             (setq pos (+ pos (* (1- chess-cn--board-grid-high) (+ 3 (* chess-cn--board-grid-width 8))))) ;; 棋盘方格高度产生的纯字符行，加上末尾的棋子位置(2个字符)和1个换行符.
             (setq row (1+ row))
             (setq col 0))
           (setq board-at-row (nth row (plist-get chess-cn--playing 'situation)))
           (setq pos (+ pos chess-cn--board-grid-offsetset)) ;; 棋盘左侧偏移
           (while (< col (car cord))
               ;; 若 (col . row) 处有棋子，则增加 chess-cn--board-grid-width - 1 个位置，否则 增加 chess-cn--board-grid-width 个位置
               (setq pos (+ pos (if (null (nth col board-at-row)) (if (= col 8) 2 chess-cn--board-grid-width) (if (= col 8) 1 (1- chess-cn--board-grid-width)))))
               (setq col (1+ col)))
           )
         pos)))


;; 未使用
(defun chess-cn--draw-situation (the-situation)
  "绘制棋局，暂未使用"
  (setq buffer-read-only nil)
  (delete-region chess-cn--board-start (1- chess-cn--board-end))
  (goto-char chess-cn--board-start)
  (while the-situation
    (insert (make-string chess-cn--board-grid-offsetset ? ))
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
             (concat (propertize (chess-cn--get-piece-name curt-piece) 'font-lock-face (chess-cn--get-piece-face curt-piece))
                     (make-string (- chess-cn--board-grid-width 2) ?-)))
            ((and with-piece is-tail) ;; 当前位置有棋子且在行尾
             ;;(message "有，yes")
             (concat (propertize (chess-cn--get-piece-name curt-piece) 'font-lock-face (chess-cn--get-piece-face curt-piece)) "\n"))
            ((and (not with-piece) (not is-tail)) ;; 当前位置无棋子且不在行尾
             ;;(message "无，no")
             (concat "+" (make-string (1- chess-cn--board-grid-width) ?-)))
            ((and (not with-piece) is-tail) ;; 当前位置无棋子且在行尾
             ;;(message "无，yes")
             "+ \n"))))
        (setq row-situation (cdr row-situation)) ;; 切换下一个棋子
        ))
    (when (cdr the-situation) ;; 棋局还有下一行，则插入中间文本行
      (insert ;; 棋局换行
       (chess-cn--scale-string
        (concat
          (make-string chess-cn--board-grid-offsetset ? )
          (chess-cn--scale-string (concat "|" (make-string (1- chess-cn--board-grid-width) ? )) 8)
          "| \n")
        (1- chess-cn--board-grid-high))))
    (setq the-situation (cdr the-situation)))
  (setq buffer-read-only t)) ;; 切换棋局下一行

(defun chess-cn--put-piece-to-board (row-situation board-row-str)
  "将棋局的一行输出到棋盘上的一行上"
  (let* ((board-row-str-len (length board-row-str))
             (max-width (if (< chess-cn--board-grid-width board-row-str-len) chess-cn--board-grid-width board-row-str-len)))
    (if row-situation
        (concat
         (if
           (car row-situation) ;; 有棋子
           (concat
             (propertize (chess-cn--get-piece-name (car row-situation)) 'font-lock-face (chess-cn--get-piece-face (car row-situation)))
             (substring board-row-str 2 max-width))
           (substring board-row-str 0 max-width))
         (chess-cn--put-piece-to-board (cdr row-situation) (substring board-row-str max-width)))
      board-row-str)))

(defun chess-cn--draw-board-by-situation (the-situation)
  "将棋局输出到棋盘"
  (setq buffer-read-only nil)
  (delete-region chess-cn--board-start (1- chess-cn--board-end))
  (goto-char chess-cn--board-start)
  (let ((i 0)
        (board-arr (split-string chess-cn--board "\n")))
    (while (< i (length board-arr))
      (insert (concat (make-string chess-cn--board-grid-offsetset ? )
                      (if (= 0 (% i 3))
          (chess-cn--put-piece-to-board (nth (/ i 3) the-situation) (nth i board-arr))
        (nth i board-arr)) "\n"))
      (setq i (1+ i))))
  (setq buffer-read-only t))

;;; }}} 字符界面表示层

;;; {{{ 内核框架
;; 缓冲区名称
(defconst chess-cn--buffer-name "*cn-chess*")

;; 对弈双方
(defconst chess-cn--side-blue '(name "蓝方" style (:background "blue")))
(defconst chess-cn--side-red '(name "红方" style (:background "red")))

(defun chess-cn--get-side-by-flag (flag)
  "根据对局方标识获取对局方信息"
  (symbol-value flag))


(defconst chess-cn--regexp-cn "[^\x00-\xff]" "中文字符正则串")


;; 兵种
(defconst chess-cn--piece-type-ju '(name (chess-cn--side-blue "車" chess-cn--side-red "車") move-rule chess-cn--move-rule-ju kill-rule chess-cn--kill-rule-ju is-king nil) "")
(defconst chess-cn--piece-type-ma '(name (chess-cn--side-blue "馬" chess-cn--side-red "馬") move-rule chess-cn--move-rule-ma kill-rule chess-cn--kill-rule-ma is-king nil) "")
(defconst chess-cn--piece-type-pao '(name (chess-cn--side-blue "砲" chess-cn--side-red "炮") move-rule chess-cn--move-rule-pao kill-rule chess-cn--kill-rule-pao is-king nil) "")
(defconst chess-cn--piece-type-bingzu '(name (chess-cn--side-blue "卒" chess-cn--side-red "兵") move-rule chess-cn--move-rule-bingzu kill-rule chess-cn--kill-rule-bingzu is-king nil) "")
(defconst chess-cn--piece-type-xiang '(name (chess-cn--side-blue "象" chess-cn--side-red "相") move-rule chess-cn--move-rule-xiang kill-rule chess-cn--kill-rule-xiang is-king nil) "")
(defconst chess-cn--piece-type-shi '(name (chess-cn--side-blue "士" chess-cn--side-red "仕") move-rule chess-cn--move-rule-shi kill-rule chess-cn--kill-rule-shi is-king nil) "")
(defconst chess-cn--piece-type-jiangshuai '(name (chess-cn--side-blue "將" chess-cn--side-red "帥") move-rule chess-cn--move-rule-jiangshuai kill-rule chess-cn--kill-rule-jiangshuai is-king t) "")

;; 蓝方棋子
(defconst chess-cn--piece-blue-jiang '(side chess-cn--side-blue type chess-cn--piece-type-jiangshuai) "蓝将")
(defconst chess-cn--piece-blue-xiang-1 '(side chess-cn--side-blue type chess-cn--piece-type-xiang) "蓝象1")
(defconst chess-cn--piece-blue-xiang-2 '(side chess-cn--side-blue type chess-cn--piece-type-xiang) "蓝象2")
(defconst chess-cn--piece-blue-shi-1 '(side chess-cn--side-blue type chess-cn--piece-type-shi) "蓝士1")
(defconst chess-cn--piece-blue-shi-2 '(side chess-cn--side-blue type chess-cn--piece-type-shi) "蓝士2")
(defconst chess-cn--piece-blue-ju-1 '(side chess-cn--side-blue type chess-cn--piece-type-ju) "蓝车1")
(defconst chess-cn--piece-blue-ju-2 '(side chess-cn--side-blue type chess-cn--piece-type-ju) "蓝车2")
(defconst chess-cn--piece-blue-ma-1 '(side chess-cn--side-blue type chess-cn--piece-type-ma) "蓝马1")
(defconst chess-cn--piece-blue-ma-2 '(side chess-cn--side-blue type chess-cn--piece-type-ma) "蓝马2")
(defconst chess-cn--piece-blue-pao-1 '(side chess-cn--side-blue type chess-cn--piece-type-pao) "蓝炮1")
(defconst chess-cn--piece-blue-pao-2 '(side chess-cn--side-blue type chess-cn--piece-type-pao) "蓝炮2")
(defconst chess-cn--piece-blue-zu-1 '(side chess-cn--side-blue type chess-cn--piece-type-bingzu) "蓝卒1")
(defconst chess-cn--piece-blue-zu-2 '(side chess-cn--side-blue type chess-cn--piece-type-bingzu) "蓝卒2")
(defconst chess-cn--piece-blue-zu-3 '(side chess-cn--side-blue type chess-cn--piece-type-bingzu) "蓝卒3")
(defconst chess-cn--piece-blue-zu-4 '(side chess-cn--side-blue type chess-cn--piece-type-bingzu) "蓝卒4")
(defconst chess-cn--piece-blue-zu-5 '(side chess-cn--side-blue type chess-cn--piece-type-bingzu) "蓝卒5")

;; 红方棋子
(defconst chess-cn--piece-red-shuai '(side chess-cn--side-red type chess-cn--piece-type-jiangshuai) "红将")
(defconst chess-cn--piece-red-xiang-1 '(side chess-cn--side-red type chess-cn--piece-type-xiang) "红象1")
(defconst chess-cn--piece-red-xiang-2 '(side chess-cn--side-red type chess-cn--piece-type-xiang) "红象2")
(defconst chess-cn--piece-red-shi-1 '(side chess-cn--side-red type chess-cn--piece-type-shi) "红士1")
(defconst chess-cn--piece-red-shi-2 '(side chess-cn--side-red type chess-cn--piece-type-shi) "红士2")
(defconst chess-cn--piece-red-ju-1 '(side chess-cn--side-red type chess-cn--piece-type-ju) "红车1")
(defconst chess-cn--piece-red-ju-2 '(side chess-cn--side-red type chess-cn--piece-type-ju) "红车2")
(defconst chess-cn--piece-red-ma-1 '(side chess-cn--side-red type chess-cn--piece-type-ma) "红马1")
(defconst chess-cn--piece-red-ma-2 '(side chess-cn--side-red type chess-cn--piece-type-ma) "红马2")
(defconst chess-cn--piece-red-pao-1 '(side chess-cn--side-red type chess-cn--piece-type-pao) "红炮1")
(defconst chess-cn--piece-red-pao-2 '(side chess-cn--side-red type chess-cn--piece-type-pao) "红炮2")
(defconst chess-cn--piece-red-bing-1 '(side chess-cn--side-red type chess-cn--piece-type-bingzu) "红卒1")
(defconst chess-cn--piece-red-bing-2 '(side chess-cn--side-red type chess-cn--piece-type-bingzu) "红卒2")
(defconst chess-cn--piece-red-bing-3 '(side chess-cn--side-red type chess-cn--piece-type-bingzu) "红卒3")
(defconst chess-cn--piece-red-bing-4 '(side chess-cn--side-red type chess-cn--piece-type-bingzu) "红卒4")
(defconst chess-cn--piece-red-bing-5 '(side chess-cn--side-red type chess-cn--piece-type-bingzu) "红卒5")

(defconst chess-cn--init-situation
  '(
   (chess-cn--piece-blue-ju-1 chess-cn--piece-blue-ma-1 chess-cn--piece-blue-xiang-1 chess-cn--piece-blue-shi-1 chess-cn--piece-blue-jiang chess-cn--piece-blue-shi-2 chess-cn--piece-blue-xiang-2 chess-cn--piece-blue-ma-2 chess-cn--piece-blue-ju-2)
   (nil nil nil nil nil nil nil nil nil)
   (nil chess-cn--piece-blue-pao-1 nil nil nil nil nil chess-cn--piece-blue-pao-2 nil)
   (chess-cn--piece-blue-zu-1 nil chess-cn--piece-blue-zu-2 nil chess-cn--piece-blue-zu-3 nil chess-cn--piece-blue-zu-4 nil chess-cn--piece-blue-zu-5)
   (nil nil nil nil nil nil nil nil nil)
   (nil nil nil nil nil nil nil nil nil)
   (chess-cn--piece-red-bing-1 nil chess-cn--piece-red-bing-2 nil chess-cn--piece-red-bing-3 nil chess-cn--piece-red-bing-4 nil chess-cn--piece-red-bing-5)
   (nil chess-cn--piece-red-pao-1 nil nil nil nil nil chess-cn--piece-red-pao-2 nil)
   (nil nil nil nil nil nil nil nil nil)
   (chess-cn--piece-red-ju-1 chess-cn--piece-red-ma-1 chess-cn--piece-red-xiang-1 chess-cn--piece-red-shi-1 chess-cn--piece-red-shuai chess-cn--piece-red-shi-2 chess-cn--piece-red-xiang-2 chess-cn--piece-red-ma-2 chess-cn--piece-red-ju-2)
   )
  "初始棋局")

(defvar chess-cn--playing '(game-over nil curt-side nil curt-selected-cord nil situation nil history nil)
  "对弈信息，包括对弈是否已结束、当前走子方、当前所选棋子的坐标、当前棋局(10x9二维棋子矩阵)")
(defun chess-cn--playing-init ()
  "对弈信息初始化"
  (plist-put chess-cn--playing 'game-over nil)
  (plist-put chess-cn--playing 'situation (chess-cn--copy-init-situation chess-cn--init-situation)) ;; 初始棋局
  (plist-put chess-cn--playing 'curt-side nil)
  (plist-put chess-cn--playing 'curt-selected-cord nil))

(defvar chess-cn--saved-dir "~/.chess" "保存棋局时默认目录")

(defun chess-cn--get-side-of-piece (chess-cn--piece)
  "获取棋子的对弈方信息"
  (chess-cn--get-side-by-flag (plist-get (symbol-value chess-cn--piece) 'side)))

(defun chess-cn--get-piece-name (chess-cn--piece)
  "获取棋子名称"
  (plist-get (plist-get (symbol-value (plist-get (symbol-value chess-cn--piece) 'type)) 'name) (plist-get (symbol-value chess-cn--piece) 'side)))

(defun chess-cn--get-piece-face (chess-cn--piece)
  "获取棋子用于显示的文本属性"
  (plist-get (symbol-value (plist-get (symbol-value chess-cn--piece) 'side)) 'style))

(defun chess-cn--copy-init-situation (chess-cn--init-situation)
  "深拷贝初始棋局"
  (if chess-cn--init-situation
      (cons (copy-list (car chess-cn--init-situation)) (chess-cn--copy-init-situation (cdr chess-cn--init-situation)))
    nil)
  )

(define-derived-mode chinese-chess-cn--mode special-mode "Chinese-Chess"
  "中国象棋主模式"
  (make-local-variable 'chess-cn--playing)
  (define-key chinese-chess-cn--mode-map (kbd "<up>") 'chess-cn--move-point-up)
  (define-key chinese-chess-cn--mode-map (kbd "<down>") 'chess-cn--move-point-down)
  (define-key chinese-chess-cn--mode-map (kbd "<left>") 'chess-cn--move-point-left)
  (define-key chinese-chess-cn--mode-map (kbd "<right>") 'chess-cn--move-point-right))

(add-hook 'chinese-chess-cn--mode-hook
          (lambda ()
            (setq-local global-hl-line-mode nil) ;; 关闭主缓冲区当前行高亮
            (setq-local cursor-type 'box)))      ;; 设置主缓冲区光标为块状


;;(add-to-list 'evil-emacs-state-modes 'chinese-chess-cn--mode)
(evil-define-key 'normal chinese-chess-cn--mode-map (kbd "RET") 'chess-cn--step-cmd)

(evil-define-key 'normal chinese-chess-cn--mode-map (kbd "<up>") 'chess-cn--move-point-up)
(evil-define-key 'normal chinese-chess-cn--mode-map (kbd "<down>") 'chess-cn--move-point-down)
(evil-define-key 'normal chinese-chess-cn--mode-map (kbd "<left>") 'chess-cn--move-point-left)
(evil-define-key 'normal chinese-chess-cn--mode-map (kbd "<right>") 'chess-cn--move-point-right)

(evil-define-key 'normal chinese-chess-cn--mode-map (kbd "k") 'chess-cn--move-point-up)
(evil-define-key 'normal chinese-chess-cn--mode-map (kbd "j") 'chess-cn--move-point-down)
(evil-define-key 'normal chinese-chess-cn--mode-map (kbd "h") 'chess-cn--move-point-left)
(evil-define-key 'normal chinese-chess-cn--mode-map (kbd "l") 'chess-cn--move-point-right)


(defun chess-cn--get-piece-from-situation (cord)
  "根据坐标获取棋局上的棋子(符号)"
  (when cord (nth (car cord) (nth (cdr cord) (plist-get chess-cn--playing 'situation)))))

(defun chess-cn--get-piece-value-from-situation (cord)
  "根据坐标获取棋局上的棋子(值)"
  (let ((piece (chess-cn--get-piece-from-situation cord)))
    (when piece (symbol-value piece))))

(defun chess-cn--set-piece-to-situation (cord piece)
  "根据坐标设置棋子(符号)"
  (setf (nth (car cord) (nth (cdr cord) (plist-get chess-cn--playing 'situation))) piece))

(defun chess-cn--get-other-side (side)
  "获取对弈对方"
  (if (eq side 'chess-cn--side-blue) 'chess-cn--side-red 'chess-cn--side-blue))

(defun chess-cn--select-piece (cord)
  "选择棋子, 更新当前所选棋子，并将允许走子方设为所选棋子所属方(以应对棋局首步棋)"
  (plist-put chess-cn--playing 'curt-selected-cord cord)
  (plist-put chess-cn--playing 'curt-side (plist-get (symbol-value (chess-cn--get-piece-from-situation cord)) 'side))
  (chess-cn--step-debug))

;; 走子
;; 走子之前先判断是否符合兵种走棋规则，包括基本规则及棋局规则
(defun chess-cn--move-piece (oldcord dstcord)
  "移动棋子"
  ;;(message (format "move piece from %s to %s" oldcord dstcord))
  (if
      (and 
       (chess-cn--move-kill-base-rule oldcord dstcord)
       (funcall
        (plist-get (symbol-value (plist-get (symbol-value (chess-cn--get-piece-from-situation oldcord)) 'type)) 'move-rule)
        oldcord
        dstcord
        (plist-get chess-cn--playing 'situation)))
      (progn
          (chess-cn--set-piece-to-situation dstcord (chess-cn--get-piece-from-situation oldcord))
          (chess-cn--set-piece-to-situation oldcord nil)
          (plist-put chess-cn--playing 'curt-selected-cord nil)
          (plist-put chess-cn--playing 'curt-side (chess-cn--get-other-side (plist-get chess-cn--playing 'curt-side)))
          (chess-cn--push-history oldcord dstcord nil)) ;; 记录棋步历史
    (message "违反走子规则")))

(defun chess-cn--kill-piece (oldcord dstcord)
  "吃子"
  ;;(message (format "kill piece %s by %s" dstcord oldcord))
  (if
      (and 
       (chess-cn--move-kill-base-rule oldcord dstcord)
       (funcall
        (plist-get (symbol-value (plist-get (symbol-value (chess-cn--get-piece-from-situation oldcord)) 'type)) 'kill-rule)
        oldcord
        dstcord
        (plist-get chess-cn--playing 'situation)))
      (progn
        (let ((killed-piece (chess-cn--get-piece-from-situation dstcord))
              (kill-piece (chess-cn--get-piece-from-situation oldcord)))
          (message (format "%s 被吃掉." killed-piece))
          (chess-cn--set-piece-to-situation dstcord kill-piece)
          (chess-cn--set-piece-to-situation oldcord nil)
          (plist-put chess-cn--playing 'curt-selected-cord nil)
          (plist-put chess-cn--playing 'curt-side (chess-cn--get-other-side (plist-get chess-cn--playing 'curt-side)))
          (chess-cn--push-history oldcord dstcord killed-piece) ;; 记录棋步历史
          (when (plist-get (symbol-value (plist-get (symbol-value killed-piece) 'type)) 'is-king) ;; 被吃掉的棋子是将帅，游戏结束
              (progn
                (plist-put chess-cn--playing 'game-over t) 
                (message (format "对弈结束, %s胜出." (plist-get (symbol-value (plist-get (symbol-value kill-piece) 'side)) 'name)))))))
    (message "违反吃子规则")))

(defun chess-cn--push-history (oldcord dstcord killed-piece)
  "记录棋步历史,killed-piece 为被吃子(符号)"
  (plist-put chess-cn--playing
             'history
             (cons (list 'oldcord oldcord 'dstcord dstcord 'killed-piece killed-piece)
                   (plist-get chess-cn--playing 'history))))

(defun chess-cn--pop-history ()
  "去掉最后一步棋步历史并返回最后一步"
  (let* ((history (plist-get chess-cn--playing 'history)))
    (plist-put chess-cn--playing 'history (cdr history))
    (car history)))

(defun chess-cn--allow-side-p (side)
  "是否为允许走子方"
  (or (null (plist-get chess-cn--playing 'curt-side)) (eq (plist-get chess-cn--playing 'curt-side) side)))

(defun chess-cn--step-cmd ()
  "走子棋步命令"
  (interactive)
  (if (plist-get chess-cn--playing 'game-over)
      (message "对弈已结束")
    (chess-cn--step)))

(defun chess-cn--step ()
  "走子棋步"
  (let ((cord (chess-cn--position-to-coordinate (point))))
        ;;(message (format "落子位置 %s, 落子处棋子 %s，当前选子位置 %s，当前选子 %s." cord (chess-cn--get-piece-from-situation cord) curt-selected-cord (chess-cn--get-piece-from-situation curt-selected-cord)))
    (if cord
        (let ((piece-at-point (chess-cn--get-piece-from-situation cord)))
          (if (plist-get chess-cn--playing 'curt-selected-cord)  ;; 当前选子非空
              (if piece-at-point ;; 光标处有棋子
                  (if (chess-cn--allow-side-p (plist-get (symbol-value piece-at-point) 'side)) (chess-cn--select-piece cord) (chess-cn--kill-piece (plist-get chess-cn--playing 'curt-selected-cord) cord))
                (chess-cn--move-piece (plist-get chess-cn--playing 'curt-selected-cord) cord))
            (if piece-at-point
                (if (chess-cn--allow-side-p (plist-get (symbol-value piece-at-point) 'side)) (chess-cn--select-piece cord) (message "无效棋步,当前应对方走子."))
              (message "无效棋步，当前未选择棋子且目标位置处无棋子."))
            ))
      (message "落子位置无效"))
    (chess-cn--draw-board-by-situation (plist-get chess-cn--playing 'situation)) ;; 重新绘制棋盘
    ;;(message (format "move point to %s" (chess-cn--coordinate-to-position cord)))
    (goto-char (chess-cn--coordinate-to-position cord)) ;; 移动光标到落子位置(有chess-cn--coordinate-to-position 有bug)
    )) 
  
(defun chess-cn--step-debug ()
  "棋步调试"
  (interactive)
  (message (format "当前走子方: %s, 当前选子: %s" (plist-get chess-cn--playing 'curt-side) (plist-get chess-cn--playing 'curt-selected-cord))))

;; }}} 内核框架

;; {{{ rule

(defun chess-cn--move-rule-stup-always-allow (oldcord dstcord situation)
  "移动规则桩，测试使用"
  t)

(defun chess-cn--kill-rule-stup-always-allow (oldcord dstcord situation)
  "吃子规则桩，测试使用"
  t)

(defun chess-cn--move-kill-base-rule (oldcord dstcord)
  "走子/吃子基本规则，不能走出棋盘范围外，不能原地踏步"
  (and (and (>= (car cord) 0) (< (car cord) 9))
       (and (>= (cdr cord) 0) (< (cdr cord) 10))
       (not (equal oldcord dstcord))))


(defun chess-cn--move-line-rule (oldcord dstcord)
  "直线判断"
  (or (equal (car oldcord) (car dstcord)) (equal (cdr oldcord) (cdr dstcord))))

(defun chess-cn--move-rule-ju (oldcord dstcord situation)
  "车的移动规则，判断 oldcord 与 dstcord 之间(不含端口)上是否有棋子，有棋子则不能移动，否则可以移动"
  (and
   (chess-cn--move-line-rule oldcord dstcord)
   (not
    (chess-cn--accumulate
     (cond
      ((equal (car oldcord) (car dstcord))
       (mapcar (lambda (x) (cons (car oldcord) x))
                (chess-cn--get-range-between (cdr oldcord) (cdr dstcord))))
      ((equal (cdr oldcord) (cdr dstcord))
       (mapcar (lambda (x) (cons x (cdr oldcord)))
               (chess-cn--get-range-between (car oldcord) (car dstcord))))
      nil)
     (lambda (cord) (chess-cn--get-piece-from-situation cord))
     nil 
     'chess-cn--or-fun))))

(defun chess-cn--kill-rule-ju (oldcord dstcord situation)
  "车的吃子规则，与移动规则相同"
  (chess-cn--move-rule-ju oldcord dstcord situation))

(defun chess-cn--move-rule-ma (oldcord dstcord situation)
  "马的移动规则"
  (cond
   ((and (equal 1 (abs (- (car oldcord) (car dstcord)))) (equal 2 (abs (- (cdr oldcord) (cdr dstcord))))) ;; 竖日字
    (not (chess-cn--get-piece-from-situation (cons (car oldcord) (/ (+ (cdr oldcord) (cdr dstcord)) 2))))) ;; 绊腿判断
   ((and (equal 2 (abs (- (car oldcord) (car dstcord)))) (equal 1 (abs (- (cdr oldcord) (cdr dstcord))))) ;; 横日字
    (not (chess-cn--get-piece-from-situation (cons (/ (+ (car oldcord) (car dstcord)) 2) (cdr oldcord))))) ;; 绊腿判断
   nil))

(defun chess-cn--kill-rule-ma (oldcord dstcord situation)
  "马的吃子规则"
  (chess-cn--move-rule-ma oldcord dstcord situation))

(defun chess-cn--move-rule-pao (oldcord dstcord situation)
  "炮的移动规则，与车相同"
  (chess-cn--move-rule-ju oldcord dstcord situation))

(defun chess-cn--kill-rule-pao (oldcord dstcord situation)
  "炮的吃子规则，需要有炮台"
  (and
   (chess-cn--move-line-rule oldcord dstcord)
   (equal
    1
    (chess-cn--accumulate
     (cond
      ((equal (car oldcord) (car dstcord))
       (mapcar (lambda (x) (cons (car oldcord) x))
                (chess-cn--get-range-between (cdr oldcord) (cdr dstcord))))
      ((equal (cdr oldcord) (cdr dstcord))
       (mapcar (lambda (x) (cons x (cdr oldcord)))
               (chess-cn--get-range-between (car oldcord) (car dstcord))))
      0)
     (lambda (cord) (if (chess-cn--get-piece-from-situation cord) 1 0))
     0 
     '+))))

(defun chess-cn--move-rule-jiangshuai (oldcord dstcord situation)
  "将帅走子规则"
  (and
   (chess-cn--range-between-p 3 5 (car dstcord) t t) ;; 不能离开九宫格
   (if
     (eq 'chess-cn--side-red (plist-get (symbol-value (chess-cn--get-piece-from-situation oldcord)) 'side))
     (chess-cn--range-between-p 7 9 (cdr dstcord) t t)
     (chess-cn--range-between-p 0 2 (cdr dstcord) t t))
   (equal 1 (+ (abs (- (car oldcord) (car dstcord))) (abs (- (cdr oldcord) (cdr dstcord))))))) ;; 只能单步走
;; TODO: 将帅不可见面

(defun chess-cn--kill-rule-jiangshuai (oldcord dstcord situation)
  "将帅吃子规则，与走子规则相同"
  (chess-cn--move-rule-jiangshuai oldcord dstcord situation))

(defun chess-cn--move-rule-shi (oldcord dstcord situation)
  "士仕走子规则"
  (and
   (chess-cn--range-between-p 3 5 (car dstcord) t t) ;; 不能离开九宫格
   (if
     (eq 'chess-cn--side-red (plist-get (symbol-value (chess-cn--get-piece-from-situation oldcord)) 'side))
     (chess-cn--range-between-p 7 9 (cdr dstcord) t t)
     (chess-cn--range-between-p 0 2 (cdr dstcord) t t))
   (equal 1 (abs (- (car oldcord) (car dstcord))))
   (equal 1 (abs (- (cdr oldcord) (cdr dstcord))))))

(defun chess-cn--kill-rule-shi (oldcord dstcord situation)
  "士仕吃子规则"
  (chess-cn--move-rule-shi oldcord dstcord situation))

(defun chess-cn--move-rule-xiang (oldcord dstcord situation)
  "象相走子规则 "
  (and
   (and ;; 田字规则
    (equal 2 (abs (- (car oldcord) (car dstcord))))
    (equal 2 (abs (- (cdr oldcord) (cdr dstcord)))))
   (not (chess-cn--get-piece-from-situation (cons (/ (+ (car oldcord) (car dstcord)) 2) (/ (+ (cdr oldcord) (cdr dstcord)) 2)))) ;; 未填心
   (if (eq 'chess-cn--side-red (plist-get (symbol-value (chess-cn--get-piece-from-situation oldcord)) 'side)) ;; 不可过河
       (chess-cn--range-between-p 5 9 (cdr dstcord) t t)
     (chess-cn--range-between-p 0 4 (cdr dstcord) t t))))

(defun chess-cn--kill-rule-xiang (oldcord dstcord situation)
  "象相吃子规则"
  (chess-cn--move-rule-xiang oldcord dstcord situation))

(defun chess-cn--move-rule-bingzu (oldcord dstcord situation)
  "兵卒走子规则"
  (if
     (eq 'chess-cn--side-red (plist-get (symbol-value (chess-cn--get-piece-from-situation oldcord)) 'side))
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

(defun chess-cn--kill-rule-bingzu (oldcord dstcord situation)
  "兵卒吃子规则"
  (chess-cn--move-rule-bingzu oldcord dstcord situation))

;; }}}

;; {{{ 光标移动命令
(defun chess-cn--move-point-to (cord)
  "移动光标"
  (goto-char (chess-cn--coordinate-to-position cord)))

(defun chess-cn--move-point-up ()
  "向上移动一格"
  (interactive)
  (chess-cn--move-point-to (chess-cn--coordinate-cycle (chess-cn--position-to-coordinate (point)) 0 -1)))

(defun chess-cn--move-point-down ()
  "向下移动一格"
  (interactive)
  (chess-cn--move-point-to (chess-cn--coordinate-cycle (chess-cn--position-to-coordinate (point)) 0 1)))

(defun chess-cn--move-point-left ()
  "向左移动一格"
  (interactive)
  (chess-cn--move-point-to (chess-cn--coordinate-cycle (chess-cn--position-to-coordinate (point)) -1 0)))

(defun chess-cn--move-point-right ()
  "向右移动一格"
  (interactive)
  (chess-cn--move-point-to (chess-cn--coordinate-cycle (chess-cn--position-to-coordinate (point)) 1 0)))

;; }}}


(defun chess-cn--new ()
  "开启新的对弈"
  (interactive)
  (get-buffer-create chess-cn--buffer-name)
  (switch-to-buffer chess-cn--buffer-name)
  (chinese-chess-cn--mode)
  (setq buffer-read-only nil)
  (erase-buffer)
  (font-lock-mode 1)
  (insert chess-cn--banner)
  (set-marker chess-cn--board-start (point)) ;; 棋盘开始位置标记
  (insert "\n")
  (set-marker chess-cn--board-end (point)) ;; 棋盘结束位置标记
  (chess-cn--playing-init) ;; 对弈信息初始化
  (chess-cn--draw-board-by-situation (plist-get chess-cn--playing 'situation)) ;; 绘制棋盘
  (chess-cn--move-point-to '(0 . 0))) ;; 初始化光标位置

(defun chess-cn--undo ()
  "悔棋"
  (interactive)
  (let ((last-history (chess-cn--pop-history))) ;; 去掉最后一步历史
    (if (not last-history)
        (message "棋步历史栈已空，无法再悔棋.")
      (plist-put chess-cn--playing 'game-over nil)
      (plist-put chess-cn--playing 'curt-selected-cord nil)
      (plist-put chess-cn--playing ;; 切换走子方
                 'curt-side
                 (chess-cn--get-other-side
                  (plist-get
                   (chess-cn--get-piece-value-from-situation
                    (plist-get last-history 'dstcord))
                   'side)))
      (chess-cn--set-piece-to-situation
       (plist-get last-history 'oldcord)
       (chess-cn--get-piece-from-situation (plist-get last-history 'dstcord)))
      (chess-cn--set-piece-to-situation
       (plist-get last-history 'dstcord)
       (plist-get last-history 'killed-piece))
      (chess-cn--draw-board-by-situation (plist-get chess-cn--playing 'situation))) ;; 绘制棋盘
))

(defun chess-cn--save ()
  "保存棋局"
  (interactive)
  (unless (file-directory-p chess-cn--saved-dir)
    (make-directory chess-cn--saved-dir t))
  (let ((now-decoded (decode-time (current-time))))
    (write-region (format "%s" chess-cn--playing) nil
                  (concat (file-name-as-directory chess-cn--saved-dir) (format-time-string "%Y-%m-%dT%H%M%S.chess")))))

(defun chess-cn--load ()
  "加载外部棋局"
  (interactive)
  (get-buffer-create chess-cn--buffer-name)
  (switch-to-buffer chess-cn--buffer-name)
  (chinese-chess-cn--mode)
  (setq buffer-read-only nil)
  (erase-buffer)
  (font-lock-mode 1)
  (insert chess-cn--banner)
  (set-marker chess-cn--board-start (point)) ;; 棋盘开始位置标记
  (insert "\n")
  (set-marker chess-cn--board-end (point)) ;; 棋盘结束位置标记
  (setq chess-cn--playing (read (chess-cn--get-string-from-file (read-file-name "请选择棋局文件" (file-name-as-directory chess-cn--saved-dir)))))
  (chess-cn--draw-board-by-situation (plist-get chess-cn--playing 'situation)) ;; 绘制棋盘
  (chess-cn--move-point-to '(0 . 0)) ;; 初始化光标位置
  )


(provide 'chess-cn)

;; end here.

