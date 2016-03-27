import functions._
import debugs._
import funcForMain._

import java.util.Scanner

object Main {
  val sc = new Scanner(System.in)
  val h = 17; val w = 14

  val strategy = Array("Collect", "Collect")  // 戦略の状態 or "Escape"


  /*  ターン処理の実行  */
  def run_turn() = {
    /*  input  */
    // ゲームの状態
    val game = new GameState(sc)
    // プレイヤーの状態
    val own = new PlayerState(sc)
    val enemy = new PlayerState(sc)


    /* process */
    val collect = new Collect(own, strategy)  // collect class
    collect.process()
    val escape = new Escape(own, strategy)
    escape.process()

    // escapeで忍術使わないならAttack呼び出し
    if(own.use_skill == None){
      val attack = new Attack(enemy, own)
      attack.process()
    }

    /* output */
    funcForMain.output(own)
  }

  /*  メインメソッド  */
  def main(args:Array[String]) = {
    // AI name
    println("Gobi")

    // game start
    while(true)
      run_turn()
  }
}
