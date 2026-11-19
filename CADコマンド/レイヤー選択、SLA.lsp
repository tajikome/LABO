;;; ============================================================
;;; SLA  ―  画層まるごと一括選択（SLAYERX 改良版・SLAのみ）
;;;   1. 基準にするオブジェクトを選択（事前選択があればそのまま使用）
;;;   2. 窓選択 … 選んだ画層のオブジェクトだけを選択
;;;   3. 窓選択で何も選択されなければ … 図面全体（モデル + 全レイアウト）から選択
;;;
;;; 窓選択中は選んだ画層以外のオブジェクトは無視されます
;;; ============================================================
(vl-load-com)

;; --- 重複排除（順序維持） ---
(defun _sla:uniq (lst / r)
  (foreach x lst (if (not (member x r)) (setq r (cons x r))))
  (reverse r)
)

;; --- ssget のワイルドカード特殊文字をエスケープ（完全一致用） ---
(defun _sla:esc (s / i c out)
  (setq out "" i 1)
  (repeat (strlen s)
    (setq c (substr s i 1))
    (if (member c '("#" "@" "." "*" "?" "~" "[" "]" "-" "," "`"))
      (setq out (strcat out "`" c))
      (setq out (strcat out c))
    )
    (setq i (1+ i))
  )
  out
)

;; --- 選択セットから画層名リストを取得 ---
(defun _sla:layers-from-ss (ss / i n lay acc)
  (setq acc '() i 0 n (sslength ss))
  (while (< i n)
    (setq lay (cdr (assoc 8 (entget (ssname ss i)))))
    (if (and lay (setq lay (vl-string-trim " " lay)) (/= lay ""))
      (setq acc (cons lay acc)))
    (setq i (1+ i))
  )
  (_sla:uniq acc)
)

;; --- 画層名 → ssget用 OR フィルタ ---
(defun _sla:layer-filter (layers / esc)
  (setq esc (mapcar '_sla:esc layers))
  (cond
    ((null esc) nil)
    ((= (length esc) 1) (list (cons 8 (car esc))))
    (t (append (list '(-4 . "<OR"))
               (mapcar '(lambda (l) (cons 8 l)) esc)
               (list '(-4 . "OR>"))))
  )
)

;; --- 区切り結合 ---
(defun _sla:join (sep lst / r)
  (if lst
    (progn
      (setq r (car lst))
      (foreach x (cdr lst) (setq r (strcat r sep x)))
      r)
    "")
)

;; --- メイン ---
(defun c:SLA (/ ss layers flt res mode)
  ;; 1. 基準オブジェクト（事前選択があればそれを使用）
  (if (not (setq ss (ssget "_I")))
    (progn
      (princ "\n基準にするオブジェクトを選択 <Enterで確定>: ")
      (setq ss (ssget))
    )
  )
  (cond
    ((null ss) (princ "\nキャンセルしました。"))
    (t
      (setq layers (_sla:layers-from-ss ss))
      (if (null layers)
        (princ "\n画層を取得できませんでした。")
        (progn
          (setq flt (_sla:layer-filter layers))

          ;; 2. 窓選択（選んだ画層のみ有効）
          (princ (strcat "\n画層 [" (_sla:join ", " layers)
                         "] を窓選択 <Enterで図面全体>: "))
          (setq res (ssget flt))
          (setq mode "窓選択")

          ;; 3. 何も選択されなければ図面全体
          (if (or (null res) (= (sslength res) 0))
            (progn
              (setq res (ssget "X" flt))
              (setq mode "図面全体")
            )
          )

          (if (and res (> (sslength res) 0))
            (progn
              (sssetfirst nil res)
              (princ
                (strcat
                  "\n● 画層: " (_sla:join ", " layers)
                  "\n● 件数: " (itoa (sslength res))
                  "  /  範囲: " mode
                )
              )
            )
            (princ "\n該当オブジェクトはありませんでした。")
          )
        )
      )
    )
  )
  (princ)
)

(princ "\n[SLA] 読込完了 → SLA:選択画層を窓選択（なければ図面全体）")
(princ)