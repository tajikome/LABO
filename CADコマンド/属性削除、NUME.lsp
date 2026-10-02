;;; ------------------------------------------------------------------
;;; コマンド名: NUME
;;; 概要: 選択したブロックの定義から属性定義をすべて削除し、
;;;       図面上のブロック参照からも属性を完全に除去します。
;;; ------------------------------------------------------------------
(defun c:NUME ( / ss i ent ename blkName doc blocks blkDef item tagList)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq blocks (vla-get-blocks doc))
  
  (princ "\n属性定義を完全に削除するブロックを選択:")
  (if (setq ss (ssget '((0 . "INSERT") (66 . 1))))
    (progn
      (vla-startundomark doc)
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq ename (vlax-ename->vla-object ent))
        (setq blkName (vla-get-effectivename ename)) ; ダイナミックブロックにも対応
        (setq blkDef (vla-item blocks blkName))
        
        ;; 1. ブロック定義からすべての属性定義(ATTDEF)を削除
        (vlax-for item blkDef
          (if (= (vla-get-objectname item) "AcDbAttributeDefinition")
            (vla-delete item)
          )
        )
        
        ;; 2. 既存のブロック参照の属性をリセット（同期）
        (command "._ATTSYNC" "_N" blkName)
      )
      (vla-endundomark doc)
      (princ "\n選択したブロックから属性を完全に削除しました。")
    )
    (princ "\n属性付きブロックが選択されませんでした。")
  )
  (princ)
)