;;; ============================================================
;;; STX / STA  ―  同一文字列一括選択（SLX/SBX の姉妹版）
;;;   STX : 現在の空間内で、選択文字と同じ文字列の TEXT/MTEXT を一括選択
;;;   STA : 図面全体（モデル + 全レイアウト）で同様に一括選択
;;;
;;; ポイント
;;;   1. 事前選択があればそのまま使用 → 追加クリック ゼロ
;;;   2. 複数の文字を基準にできる（複数文字列 OR 選択）
;;;   3. MTEXT の書式コード({...} や \P、%%u 等)を除去して比較するため、
;;;      見た目が同じなら書式違いでもヒットする
;;;   4. 大文字小文字・前後の空白は無視して比較
;;;   5. 文字列ごとの件数内訳を表示 → 簡易カウンターとしても使える
;;; ============================================================
(vl-load-com)

;; --- 重複排除（順序維持） ---
(defun _stx:uniq (lst / r)
  (foreach x lst (if (not (member x r)) (setq r (cons x r))))
  (reverse r)
)

;; --- 区切り結合 ---
(defun _stx:join (sep lst / r)
  (if lst
    (progn
      (setq r (car lst))
      (foreach x (cdr lst) (setq r (strcat r sep x)))
      r)
    "")
)

;; --- カウントアップ ---
(defun _stx:inc (key lst / a)
  (if (setq a (assoc key lst))
    (subst (cons key (1+ (cdr a))) a lst)
    (cons (cons key 1) lst)
  )
)

;; --- MTEXT書式コード除去 + 正規化（大文字化・前後空白除去） ---
;;   除去対象: {,}  \P(改行→空白)  \L \O \W1.0; などの制御列  %%u %%o
(defun _stx:norm (s / i c nxt out len)
  (if (null s) (setq s ""))
  (setq out "" i 1 len (strlen s))
  (while (<= i len)
    (setq c (substr s i 1))
    (cond
      ;; { } は捨てる
      ((member c '("{" "}"))
       (setq i (1+ i)))
      ;; \\ → \ 、\{ → { として1文字採用
      ((and (= c "\\") (member (substr s (1+ i) 1) '("\\" "{" "}")))
       (setq out (strcat out (substr s (1+ i) 1)))
       (setq i (+ i 2)))
      ;; \P は空白に置換（改行扱い）
      ((and (= c "\\") (member (strcase (substr s (1+ i) 1)) '("P")))
       (setq out (strcat out " "))
       (setq i (+ i 2)))
      ;; \f...; \H...; \W...; \C...; \T...; \Q...; \A...; などは ; まで捨てる
      ((and (= c "\\")
            (member (strcase (substr s (1+ i) 1))
                    '("F" "H" "W" "C" "T" "Q" "A" "S" "K" "X" "PI" "O" "L")))
       (setq nxt (1+ i))
       (while (and (<= nxt len) (/= (substr s nxt 1) ";"))
         (setq nxt (1+ nxt)))
       (setq i (1+ nxt)))
      ;; %%u %%o %%d 等の旧形式コード
      ((and (= c "%") (= (substr s (1+ i) 1) "%"))
       (setq i (+ i 3)))
      (t
       (setq out (strcat out c))
       (setq i (1+ i)))
    )
  )
  (strcase (vl-string-trim " " out))
)

;; --- エンティティから正規化済み文字列を取得 ---
(defun _stx:text-of (en)
  (_stx:norm (vla-get-TextString (vlax-ename->vla-object en)))
)

;; --- 選択セットから TEXT/MTEXT のみ抽出して文字列リストを取得 ---
(defun _stx:texts-from-ss (ss / i n en acc s)
  (setq acc '() i 0 n (sslength ss))
  (while (< i n)
    (setq en (ssname ss i))
    (if (member (cdr (assoc 0 (entget en))) '("TEXT" "MTEXT"))
      (progn
        (setq s (_stx:text-of en))
        (if (/= s "") (setq acc (cons s acc)))
      )
    )
    (setq i (1+ i))
  )
  (_stx:uniq acc)
)

;; --- 共通処理   space-mode: "CUR"=現在空間 / "ALL"=図面全体 ---
(defun _stx:run (space-mode / ss strs cand i n en s res bycnt)
  ;; 事前選択があればそれを使用、無ければ文字選択を促す
  (if (not (setq ss (ssget "_I")))
    (progn
      (princ "\n基準にする文字を選択 <Enterで確定>: ")
      (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    )
  )
  (cond
    ((null ss) (princ "\nキャンセルしました。"))
    (t
      (setq strs (_stx:texts-from-ss ss))
      (if (null strs)
        (princ "\n選択の中に文字がありませんでした。")
        (progn
          ;; 候補: 空間内(or 全体)の全 TEXT/MTEXT を取得し、正規化文字列で照合
          (setq cand
            (ssget "X"
              (if (= space-mode "ALL")
                '((0 . "TEXT,MTEXT"))
                (list '(0 . "TEXT,MTEXT") (cons 410 (getvar "CTAB")))
              )
            )
          )
          (setq res (ssadd) bycnt nil)
          (if cand
            (progn
              (setq i 0 n (sslength cand))
              (while (< i n)
                (setq en (ssname cand i))
                (setq s (_stx:text-of en))
                (if (member s strs)
                  (progn
                    (ssadd en res)
                    (setq bycnt (_stx:inc s bycnt))
                  )
                )
                (setq i (1+ i))
              )
            )
          )
          (if (> (sslength res) 0)
            (progn
              (sssetfirst nil res)
              (princ
                (strcat
                  "\n● 文字列: \"" (_stx:join "\", \"" strs) "\""
                  "\n● 合計: " (itoa (sslength res)) " 個"
                  "  /  範囲: "
                  (if (= space-mode "ALL")
                    "図面全体"
                    (strcat "空間「" (getvar "CTAB") "」"))
                )
              )
              (if (> (length bycnt) 1)
                (foreach x bycnt
                  (princ (strcat "\n    " (itoa (cdr x)) " x " (car x)))
                )
              )
            )
            (princ "\n該当文字はありませんでした。")
          )
        )
      )
    )
  )
  (princ)
)

;; --- コマンド定義 ---
(defun c:STX () (_stx:run "CUR"))   ; 現在の空間のみ（既定・最少クリック）
(defun c:STA () (_stx:run "ALL"))   ; 図面全体（モデル+全レイアウト）

(princ "\n[同一文字列選択] 読込完了 → STX:現在空間 / STA:図面全体")
(princ)
