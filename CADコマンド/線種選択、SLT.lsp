(defun c:SLT ( / sEnt eData sLt ss)
  (princ "\n--- 線種による一括選択 ---")
  ;; 基準となるオブジェクトを選択
  (setq sEnt (entsel "\n基準となる線種のオブジェクトを選択: "))
  (if sEnt
    (progn
      (setq eData (entget (car sEnt)))
      ;; オブジェクトの線種属性（DXF 6）を取得。指定がない場合は "ByLayer"
      (if (assoc 6 eData)
        (setq sLt (cdr (assoc 6 eData)))
        (setq sLt "ByLayer")
      )
      
      (princ (strcat "\n選択された線種: " sLt))
      (princ "\n範囲を選択（Enterで図面全体）: ")
      
      ;; まず通常の範囲選択を試みる
      (setq ss (ssget (list (cons 6 sLt))))
      
      ;; 範囲指定されずEnterが押された場合は、図面全体("_A")から選択
      (if (null ss)
        (setq ss (ssget "_A" (list (cons 6 sLt))))
      )
      
      (if ss
        (progn
          (sssetfirst nil ss) ; 選択状態（ハイライト）にする
          (princ (strcat "\n" (itoa (sslength ss)) " 個のオブジェクトを選択しました。"))
        )
        (princ "\n該当するオブジェクトは見つかりませんでした。")
      )
    )
    (princ "\nオブジェクトが選択されませんでした。")
  )
  (princ)
)
(princ "\nコマンド「SLT」で実行できます。")
(princ)