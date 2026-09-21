(defun c:REBL ( / ssPre targetEnt targetName ss i entName elist )
  (vl-load-com)
  
  ;; 1. 基準となるブロックの取得（事前選択されているかチェック）
  (setq ssPre (ssget "I" '((0 . "INSERT")))) ; 事前選択（Pickfirst）されているブロックを取得
  
  (if (and ssPre (= (sslength ssPre) 1))
    ;; パターンA：すでに1つだけブロックが選択されている場合、それを基準にする
    (progn
      (setq targetEnt (ssname ssPre 0))
      (setq targetName (cdr (assoc 2 (entget targetEnt))))
      (princ (strcat "\n[基準ブロック] 事前選択されたブロックを使用します: " targetName))
      ;; 事前選択の選択状態を一度クリア
      (sssetfirst nil nil)
    )
    ;; パターンB：何も選択されていない、または複数選択されている場合は、新しく1つ選択させる
    (progn
      (if ssPre (sssetfirst nil nil)) ; 選択状態を解除
      (setq targetEnt (entsel "\n[1/2] 基準となるブロックを選択してください: "))
      (if (not targetEnt)
        (progn (princ "\n選択がキャンセルされました。") (exit))
      )
      (if (= (cdr (assoc 0 (entget (car targetEnt)))) "INSERT")
        (progn
          (setq targetEnt (car targetEnt))
          (setq targetName (cdr (assoc 2 (entget targetEnt))))
          (princ (strcat "\n置換先ブロック名: " targetName))
        )
        (progn
          (princ "\nエラー: 選択されたオブジェクトはブロックではありません。")
          (exit)
        )
      )
    )
  )

  ;; 2. 置き換えたいブロックを選択（個別クリック、または窓選択で複数選択可）
  (princ "\n[2/2] 置き換えたいブロックを選択してください（個別クリック / 窓選択）: ")
  (setq ss (ssget '((0 . "INSERT"))))
  
  (if (not ss)
    (progn (princ "\n選択されませんでした。") (exit))
  )

  ;; 3. 選択されたブロックをまとめて置換（自分自身は除外する）
  (setq i 0)
  (while (< i (sslength ss))
    (setq entName (ssname ss i))
    ;; 基準ブロック自身が巻き込まれていた場合は置換しないように除外
    (if (/= entName targetEnt)
      (progn
        (setq elist (entget entName))
        ;; ブロック名を置換先の名前に書き換え
        (setq elist (subst (cons 2 targetName) (assoc 2 elist) elist))
        (entmod elist)
        (entupd entName)
      )
    )
    (setq i (1+ i))
  )
  
  (princ (strcat "\n完了: ブロックを [" targetName "] に置き換えました。"))
  (princ)
)

(princ "\n--- ブロック置換LISPロード完了 (コマンド: REBL) ---")
(princ)