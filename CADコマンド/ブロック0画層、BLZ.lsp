(defun c:BLZ (/ ss i ent blkname processed-blocks)
  (vl-load-com)
  (princ "\n選択したブロックの全階層を 0画層 / ByBlock にします。")

  ;; 既に処理したブロック名を記録するリスト（無限ループ防止）
  (setq processed-blocks '())

  ;; ブロック定義を再帰的に書き換えるサブ関数
  (defun _ProcessBlockDefinition (blkname / btr ename ed sub-blkname)
    (if (not (member blkname processed-blocks))
      (progn
        (setq processed-blocks (cons blkname processed-blocks))
        (setq btr (tblobjname "BLOCK" blkname))
        (setq ename (entnext btr))
        
        (while (and ename (/= "ENDBLK" (cdr (assoc 0 (setq ed (entget ename))))))
          ;; 1. 画層を0に
          (if (assoc 8 ed)
            (setq ed (subst (cons 8 "0") (assoc 8 ed) ed))
            (setq ed (append ed (list (cons 8 "0"))))
          )
          ;; 2. 色をByBlock (0) に変更
          (if (assoc 62 ed)
            (setq ed (subst (cons 62 0) (assoc 62 ed) ed))
            (setq ed (append ed (list (cons 62 0))))
          )
          ;; 3. TrueColorなどの削除
          (foreach code '(420 430 440)
            (if (assoc code ed) (setq ed (vl-remove (assoc code ed) ed)))
          )
          ;; 4. 線種をByLayer
          (if (assoc 6 ed)
            (setq ed (subst (cons 6 "BYLAYER") (assoc 6 ed) ed))
            (setq ed (append ed (list (cons 6 "BYLAYER"))))
          )
          ;; 5. 線の太さをByLayer (-1)
          (if (assoc 370 ed)
            (setq ed (subst (cons 370 -1) (assoc 370 ed) ed))
            (setq ed (append ed (list (cons 370 -1))))
          )
          
          (entmod ed)

          ;; --- 入れ子対応: もし要素がブロック参照なら、その定義も処理する ---
          (if (= "INSERT" (cdr (assoc 0 ed)))
            (progn
              (setq sub-blkname (cdr (assoc 2 ed)))
              (_ProcessBlockDefinition sub-blkname)
            )
          )

          (setq ename (entnext ename))
        )
      )
    )
  )

  ;; メイン処理
  (setq ss (ssget '((0 . "INSERT"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq blkname (cdr (assoc 2 (entget ent))))
        (_ProcessBlockDefinition blkname)
        (setq i (1+ i))
      )
      (command-s "._REGEN")
      (princ "\n全階層の処理が完了しました。")
    )
    (princ "\nブロック参照が選択されていません。")
  )
  (princ)
)