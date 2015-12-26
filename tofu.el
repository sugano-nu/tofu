;; # -*- coding: utf-8 -*-
;; tofu.el
;; イスカンダルのトーフ屋ゲーム Emacs Lisp版 Ver. 0.90
;; Emacs Lisp version by SUGANO Tsuyoshi
;; 2015-12-26
;;
;; イスカンダルのトーフ屋ゲーム
;; http://vivi.dyndns.org/tofu/tofu.html
;; Copyright (C) 1978 - 2015 by Nobuhide Tsuda
;; CAUTION: Public release and redistribution of translation of Tofu game program codes
;;          without permission of the original author Nobuhide Tsuda (ntsuda@master.email.ne.jp) is strictly prohibited.
;; 源著作権は津田伸秀さん (ntsuda@master.email.ne.jp) にあります。
;; 注意：「源著作権者に無断で移植し，公開または第三者に配布することは禁止します．」
;;
;; オリジナルの考案・開発者の津田伸秀さんに感謝し、敬意を表します。
;; また、 Common Lisp 版を作成された 竹岡尚三さんに敬意を表します。
;; Common Lisp version by Takeoka Shozo
;; http://www.takeoka.org/~take/ailabo/gcl/tofu-lisp.html
;;
;; 遊び方：
;;   (load-file "tofu.el") で Emacs へ読み込み、
;;   M-x eval-buffer で読み込んだバッファを評価し、
;;   M-x tofu で実行します。
;; UIの変更点：
;;   Emacs 24 以降では、画面の文字表示の大きさを選択できます。
;;   晴れ、曇り、雨など、文字表記の一部をカラー化しています。
;;   コンピュータ側の動作を「ＣＰＵ」と表記しています。
;; その他：
;;   使用に際して不具合や事故が起こっても原作・著作権者、移植者はなんら保障を行ないません。あらかじめ御了承ください。

(require 'cl)
(defvar *player*)
(defvar *comp*)

(defvar *hare*)
(defvar *kumori*)
(defvar *ame*)
(defvar *slow-tty* t)

(defvar *rand* (make-random-state t ))

(defun random100 ()
  (random 101))

(defun disp (str)
  (interactive)
  (set-buffer (get-buffer-create "*tofu*"))
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (dotimes (i (length str))
      (insert (format "%s" (char-to-string (aref str i))))
      (if *slow-tty* (sit-for 0.001))
      )))

(defun disp-color (str color)
  (interactive)
  (set-buffer (get-buffer-create "*tofu*"))
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (dotimes (i (length str))
      (insert (propertize (format "%s" (char-to-string (aref str i)))
                          'face
                          ;;`(:foreground ,color :background "black")))
                          `(:foreground ,color)))
      (if *slow-tty* (sit-for 0.001))
      )))
(defun disp-green   (str) (disp-color str "green"))
(defun disp-pink    (str) (disp-color str "pink"))
(defun disp-yellow  (str) (disp-color str "yellow"))
(defun disp-gray    (str) (disp-color str "gray"))
(defun disp-cyan    (str) (disp-color str "cyan"))
(defun disp-blue    (str) (disp-color str "blue"))
(defun disp-red     (str) (disp-color str "red"))
(defun disp-magenta (str) (disp-color str "magenta"))

(defun calc()
  (floor
   (min (/ *comp*  40)
        (if(>= *hare*  50)  500 
          (if(> *ame*  30) 100
            300)))))

(defun dispRule ()
  (disp
   "\n\nここはイスカンダル星。あなたはここでトーフ屋を経営し、
地球への帰還費用を作り出さなくてはいけません。
でもお向かいには、コンピュータが経営するトーフ屋があります。。。

トーフの原価は１個４０円、販売価格は５０円です。
１日に売れる個数は天候に左右されます。
晴れると５００個、くもりだと３００個、雨のときは１００個まで売れます。
トーフは日持ちしないので、売れ残った分はすべて廃棄します。
そこで、次の日の天気予報をよく見て、何個作るか決心してください。
所持金５千円からはじめて早く３万円を超えた方が勝ちです。\n"
   ))

(defun dispShojikin (name gold)
  (let ((ggg (floor(/ gold 1000))) )
    (if (string-equal name "あなた")
        (progn
          (disp-green (format "  - %s %5d 円  " name gold))
          (disp-green (format "%s%s\n"
                              (apply 'concat (make-list ggg "#"))
                              (apply 'concat (make-list (max (- 30 ggg) 0) "_")
                                     )))
          )
      )
    (if (string-equal name "ＣＰＵ")
        (progn
          (disp-pink (format "  - %s %5d 円  " name gold))
          (disp-pink (format "%s%s\n"
                             (apply 'concat (make-list ggg "#"))
                             (apply 'concat (make-list (max (- 30 ggg) 0) "_")
                                    )))
          )
      )
    ))

(defvar  *prob1*)
(defvar  *prob2*)
(defun yohou ()
  (setq *prob1* (random100))
  (setq *prob2* (random100))
  (setq *hare* 
        (- 100 (max *prob1* *prob2*)))
  (setq *ame* (min *prob1* *prob2*))
  (setq *kumori* (- 100 *hare* *ame*)))

(defun dispYohou (hare kumori ame)
  (let* ((hhh (floor (/ (* hare 10) 25)))
         (kkk (floor (/ (* kumori 10) 25)))
         (aaa (- (/ 1000 25) (+ hhh kkk))))

    (disp "\n* 明日の天気予報： ")
    (disp-yellow (format "晴れ %d  "   hare))
    (disp-gray   (format "くもり %d  " kumori))
    (disp-cyan   (format "雨 %d\n"     ame))

    ;;(disp-yellow (apply 'concat (make-list hhh "◎")))
    (disp "  ")
    (disp-yellow (apply 'concat (make-list hhh "O")))
    ;;(disp-gray (apply 'concat (make-list kkk "・")))
    (disp-gray (apply 'concat (make-list kkk ".")))
    ;;(disp-cyan (apply 'concat (make-list aaa "▲")))
    (disp-cyan (apply 'concat (make-list aaa "/")))
    (disp "\n")
    ))

(defun* input-text-scale ()
  (loop
   (text-scale-increase 0)
   (disp "\n画面表示を拡大しますか？ [0(このまま)、1〜4（より拡大）] ")
   (setq text-scale
         (string-to-number
          (read-string "画面表示を拡大しますか？ [0(このまま)、1〜4（より拡大）] ")))
   (text-scale-increase text-scale)

   (disp "\n\nこの大きさでいいですか？ [y / n（設定し直し）] ")
   (if (string-equal
        "y"
        (read-string "この設定でいいですか？ [y（ゲーム続行）/ n（設定し直し）] "))
       (return-from input-text-scale))
   )
  )

(defun tofu ()
  (interactive)
  (delete-other-windows)
  ;;(split-window-right)
  (switch-to-buffer (get-buffer-create "*tofu*"))
  (toggle-read-only nil)
  (erase-buffer)
  (toggle-read-only 1)

  (save-excursion
    (let (man)
      (disp
       (format "%s\n%s\n%s\n%s\n\n"
               "イスカンダルのトーフ屋ゲーム (Emacs Lisp版)"
               "Copyright (C) 1978 - 2015 by Nobuhide Tsuda"
               "Common Lisp version by Takeoka Shozo"
               "Emacs Lisp version by SUGANO Tsuyoshi"))

      (if (string-match "^24\." emacs-version)
          (input-text-scale))

      (disp (format "\n%s" "一文字ずつ文字表示をしますか？[y/n] "))
      (if (string-equal
           "y"
           (read-string "一文字ずつ文字表示をしますか？[y/n] "))
          (setq *slow-tty* t)
        (setq *slow-tty* nil))

      (disp (format "\n%s" "ルール説明しますか？[y/n] "))
      (if (string-equal
           "y"
           (read-string "ルール説明しますか？[y/n] "))
          (dispRule))
      (tofux))
    ))

(defun tofux ()
  (loop
   (tofu1)
   (cond
    ((> *player* *comp*)
     (disp (format "\n - あなたの勝ちです。")))
    ((eql *player* *comp*)
     (disp (format "\n - 引き分けです。")))
    (t
     (disp (format "\n - ＣＰＵの勝ちです。"))))

   (disp (format "\n - play again ? [y/n] "))
   (if (not (string-equal
             "y"
             (read-string "play again ? [y/n] ")))
       (return)
     )))

(defun inpTofu (limit)
  (let (n)
    (loop
     (disp (format "\n* トーフを何個作りますか？（１〜%s個まで）>" limit))
     (setq n (string-to-number (read-string "トーフを何個作りますか？ ")))
     (if (and (numberp n) (> n 0) (<= n limit))
         (return n)))))

(defmacro profit (n sold)
  `(- (* (min ,sold ,n) 50) (* ,n  40)))

(defun tofu1 ()
  (let (limit n)
    (setq *player* 5000)
    (setq *comp* 5000)

    (loop
     (disp (format "\n* 所持金：\n"))
     (dispShojikin "あなた" *player*)
     (dispShojikin "ＣＰＵ" *comp*)
   
     (if (or (>= *player* 30000)(>= *comp* 30000))
         (return))

     ;; 天気予報
     (yohou)
     (dispYohou *hare* *kumori* *ame*)

     ;; 作る数を入力
     (setq limit (floor (/ *player* 40)))
     (setq human (inpTofu limit))
     (disp-green (format "\n  - あなたは %s個 作ります。\n" human))

     (setq comp (calc))
     (disp-pink (format "  - ＣＰＵは %s個 作ります。\n\n" comp))
     (sit-for 0.001)

     (disp ";; ＊＊＊＊＊ 次の日 ＊＊＊＊＊")
     (setq r (random100))

     (cond
      ((<= r *ame*)
       (setq tenki " 雨  (;_;) ")
       (setq sold 100))
      ((<= r (+ *ame* *kumori*))
       (setq tenki " くもり  (~_~) ")
       (setq sold 300))
      (t
       (setq tenki " 晴れ ＼(^o^)／ ")
       (setq sold 500)))
   
     (disp (format "\n  - 今日の天気は"))
     (dotimes (i 3)
       (sit-for 0.2)
       (disp " ."))

     (sit-for 0.001)

     (disp (format "%s です。" tenki))
     (disp (format "sold=%s\n" sold))

     (incf *player*
           (profit human sold))
     (incf *comp*
           (profit comp sold))

     (sit-for 0.001)
     (goto-char (point-max))
     )))
