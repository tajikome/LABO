;;; ============================================================
;;; 同一ブロック一括選択
;;;   SBW : 窓選択（指定した範囲内）で選択ブロックと同名のブロックを一括選択
;;;   SBX : 現在の空間（表示中のタブ）全体で一括選択
;;;   SBA : 図面全体（モデル + 全レイアウト）で一括選択
;;;
;;; ポイント
;;;   1. 事前選択があればそのまま使用 → 基準ブロックの追加クリックゼロ
;;;   2. 複数ブロックをまとめて基準にできる
;;;   3. ダイナミックブロック対応 (EffectiveName で照合)
;;;   4. ブロック名ごとの件数内訳を表示
;;; ============================================================
(vl-load-com)

;; --- 重複排除（順序維持） ---
(defun _sbx:uniq (lst / r)
  (foreach x lst (if (not (member x r)) (setq r (cons x r))))
  (reverse r)
)

;; --- 区切り結合 ---
(defun _sbx:join (sep lst / r)
  (if lst
    (progn
      (setq r (car lst))
      (foreach x (cdr lst) (setq r (strcat r sep x)))
      r)
    "")
)

;; --- ダイナミックブロック対応のブロック名取得（大文字化） ---
(defun _sbx:eff-name (en / o nm)
  (setq o (vlax-ename->vla-object en))
  (setq nm
    (if (vlax-property-available-p o 'EffectiveName)
      (vla-get-EffectiveName o)
      (cdr (assoc 2 (entget en)))
    )
  )
  (strcase nm)
)

;; --- 選択セットから INSERT のみ抽出してブロック名リストを取得 ---
(defun _sbx:names-from-ss (ss / i n en acc)
  (setq acc '() i 0 n (sslength ss))
  (while (< i n)
    (setq en (ssname ss i))
    (if (= "INSERT" (cdr (assoc 0 (entget en))))
      (setq acc (cons (_sbx:eff-name en) acc))
    )
    (setq i (1+ i))
  )
  (_sbx:uniq acc)
)

;; --- カウントアップ ---
(defun _sbx:inc (key lst / a)
  (if (setq a (assoc key lst))
    (subst (cons key (1+ (cdr a))) a lst)
    (cons (cons key 1) lst)
  )
)

;; --- 共通処理   space-mode: "WIN"=窓選択 / "CUR"=現在空間 / "ALL"=図面全体 ---
(defun _sbx:run (space-mode / ss names cand i n en nm res bycnt msg)
  ;; 事前選択があればそれを使用、無ければブロック選択を促す
  (if (not (setq ss (ssget "_I")))
    (progn
      (princ "\n基準にするブロックを選択 <Enterで確定>: ")
      (setq ss (ssget '((0 . "INSERT"))))
    )
  )
  (cond
    ((null ss) (princ "\nキャンセルしました。"))
    (t
      (setq names (_sbx:names-from-ss ss))
      (if (null names)
        (princ "\n選択の中にブロックがありませんでした。")
        (progn
          ;; 候補: モードに応じて対象範囲内の INSERT を取得
          (setq cand
            (cond
              ((= space-mode "ALL")
               (ssget "X" '((0 . "INSERT")))
              )
              ((= space-mode "WIN")
               (princ (strcat "\n検索範囲を選択してください (" (_sbx:join ", " names) "): "))
               (ssget '((0 . "INSERT")))
              )
              (t ;; CUR
               (ssget "X" (list '(0 . "INSERT") (cons 410 (getvar "CTAB"))))
              )
            )
          )
          
          (setq res (ssadd) bycnt nil)
          (if cand
            (progn
              (setq i 0 n (sslength cand))
              (while (< i n)
                (setq en (ssname cand i))
                (setq nm (_sbx:eff-name en))
                (if (member nm names)
                  (progn
                    (ssadd en res)
                    (setq bycnt (_sbx:inc nm bycnt))
                  )
                )
                (setq i (1+ i))
              )
            )
          )
          (if (> (sslength res) 0)
            (progn
              (sssetfirst nil res)
              (setq msg
                (strcat
                  "\n● ブロック: " (_sbx:join ", " names)
                  "\n● 合計: " (itoa (sslength res)) " 個"
                  "  /  範囲: "
                  (cond
                    ((= space-mode "ALL") "図面全体")
                    ((= space-mode "WIN") "窓選択範囲")
                    (t (strcat "空間「" (getvar "CTAB") "」"))
                  )
                )
              )
              (princ msg)
              (if (> (length bycnt) 1)
                (foreach x bycnt
                  (princ (strcat "\n    " (itoa (cdr x)) " x " (car x)))
                )
              )
            )
            (princ "\n該当ブロックはありませんでした。")
          )
        )
      )
    )
  )
  (princ)
)

;; --- コマンド定義 ---
(defun c:SBW () (_sbx:run "WIN"))   ; 窓選択（ユーザーが範囲指定）
(defun c:SBX () (_sbx:run "CUR"))   ; 現在の空間全体（クリック不要で全選択）
(defun c:SBA () (_sbx:run "ALL"))   ; 図面全体（モデル+全レイアウト）

(princ "\n[同一ブロック一括選択] 読込完了")
(princ "\nコマンド: SBW(窓選択) / SBX(現在の空間) / SBA(図面全体)")
(princ)