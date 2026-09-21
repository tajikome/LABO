;;; ============================================================
;;; SLX / SLA  ―  画層まるごと一括選択（SLAYERX 改良版）
;;;   SLX : 現在の空間内で、選択オブジェクトと同じ画層を一括選択
;;;   SLA : 図面全体（モデル + 全レイアウト）で同様に一括選択
;;;
;;; 改良ポイント
;;;   1. モード質問を廃止 … 現在の空間(CTAB)を自動判定 → クリック削減
;;;   2. 事前選択があればそのまま使用 → 追加クリック ゼロ
;;;   3. 複数オブジェクト / 複数画層をまとめて指定可能
;;;   4. 画層名のワイルドカード文字をエスケープして誤マッチ防止
;;; ============================================================
(vl-load-com)

;; --- 重複排除（順序維持） ---
(defun _slx:uniq (lst / r)
  (foreach x lst (if (not (member x r)) (setq r (cons x r))))
  (reverse r)
)

;; --- ssget のワイルドカード特殊文字をエスケープ（完全一致用） ---
(defun _slx:esc (s / i c out)
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
(defun _slx:layers-from-ss (ss / i n lay acc)
  (setq acc '() i 0 n (sslength ss))
  (while (< i n)
    (setq lay (cdr (assoc 8 (entget (ssname ss i)))))
    (if (and lay (setq lay (vl-string-trim " " lay)) (/= lay ""))
      (setq acc (cons lay acc)))
    (setq i (1+ i))
  )
  (_slx:uniq acc)
)

;; --- 画層名 → ssget用 OR フィルタ ---
(defun _slx:layer-filter (layers / esc)
  (setq esc (mapcar '_slx:esc layers))
  (cond
    ((null esc) nil)
    ((= (length esc) 1) (list (cons 8 (car esc))))
    (t (append (list '(-4 . "<OR"))
               (mapcar '(lambda (l) (cons 8 l)) esc)
               (list '(-4 . "OR>"))))
  )
)

;; --- 区切り結合（vl-string-join フォールバック） ---
(defun _slx:join (sep lst / r)
  (if lst
    (progn
      (setq r (car lst))
      (foreach x (cdr lst) (setq r (strcat r sep x)))
      r)
    "")
)

;; --- 共通処理   space-mode: "CUR"=現在空間 / "ALL"=図面全体 ---
(defun _slx:run (space-mode / ss layers flt res)
  ;; 事前選択があればそれを使用、無ければ選択を促す
  (if (not (setq ss (ssget "_I")))
    (progn
      (princ "\n基準にするオブジェクトを選択 <Enterで確定>: ")
      (setq ss (ssget))
    )
  )
  (cond
    ((null ss) (princ "\nキャンセルしました。"))
    (t
      (setq layers (_slx:layers-from-ss ss))
      (if (null layers)
        (princ "\n画層を取得できませんでした。")
        (progn
          (setq flt
            (if (= space-mode "ALL")
              (_slx:layer-filter layers)
              (append (list '(-4 . "<AND") (cons 410 (getvar "CTAB")))
                      (_slx:layer-filter layers)
                      (list '(-4 . "AND>")))
            )
          )
          (setq res (ssget "X" flt))
          (if (and res (> (sslength res) 0))
            (progn
              (sssetfirst nil res)
              (princ
                (strcat
                  "\n● 画層: " (_slx:join ", " layers)
                  "\n● 件数: " (itoa (sslength res))
                  "  /  範囲: "
                  (if (= space-mode "ALL")
                    "図面全体"
                    (strcat "空間「" (getvar "CTAB") "」"))
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

;; --- コマンド定義 ---
(defun c:SLX () (_slx:run "CUR"))   ; 現在の空間のみ（既定・最少クリック）
(defun c:SLA () (_slx:run "ALL"))   ; 図面全体（モデル+全レイアウト）

(princ "\n[SLAYERX 改] 読込完了 → SLX:現在空間 / SLA:図面全体")
(princ)
