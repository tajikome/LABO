(defun c:SF ( / ss )
  (vl-load-com)
  
  (princ "\n=== プレビュー付き 仮起点(FROM)ストレッチ ===")
  (princ "\nストレッチするオブジェクトを選択（交差窓などで）:")
  
  ;; 1. オブジェクトの選択
  (setq ss (ssget))
  
  (if ss
    (progn
      (princ "\n【操作】 1.基準点をクリック ➔ 2.仮起点(壁など)をクリック ➔ 3.方向を向けて距離を入力")
      
      ;; 2. STRETCH実行の流れ:
      ;; _.stretch -> 選択セット(ss) -> 選択完了("")
      ;; -> 1点目(基準点)の指定(pause)
      ;; -> 2点目の指定時に "_from" を割り込み
      ;; -> あとはユーザーに操作を任せる (cmdactiveが続く限りpause)
      (command "_.stretch" ss "" pause "_from")
      (while (= (getvar "cmdactive") 1)
        (command pause)
      )
    )
    (princ "\nオブジェクトが選択されませんでした。")
  )
  (princ)
)

(princ "\n[SF] プレビュー対応版がロードされました。")
(princ)