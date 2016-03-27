import functions._
import debugs._

import java.util.Scanner


/*  プレイヤーの状態  */
class PlayerState(sc: java.util.Scanner) {
  val ninryoku = sc.nextInt()  // 忍力のストック数

  // 地図情報
  val h = sc.nextInt(); val w = sc.nextInt() // サイズ(h=17, w=14)
  val chizu = new Array[String](h)  // 地図

  sc.nextLine(); for(i <- 0 until h)chizu(i) = sc.nextLine(); sc.nextLine()


  // 忍者情報
  val ninjas = new Array[(Int, Int)](2)  // 忍者の座標(x, y)

  for(i <- 0 until 2){
    sc.nextInt(); val y = sc.nextInt(); val x = sc.nextInt()
    ninjas(i) = (x, y)
  }


  // 忍犬情報
  val dog_num = sc.nextInt()  // 忍犬数
  val dogs = new Array[(Int, Int)](dog_num)  // 忍犬の座標(x, y)

  for(i <- 0 until dog_num){
    sc.nextInt(); val y = sc.nextInt(); val x = sc.nextInt()
    dogs(i) = (x, y)
  }


  // 忍者ソウル情報
  val soul_num = sc.nextInt()  // 忍者ソウルの配置数
  val souls = new Array[(Int, Int)](soul_num)  // 忍者ソウルの座標(x, y)

  for(i <- 0 until soul_num){
    val y = sc.nextInt(); val x = sc.nextInt()
    souls(i) = (x, y)
  }


  // 忍術の使用回数
  val skill_used = new Array[Int](8)    // 忍術の使用回数

  for(i <- 0 until 8)skill_used(i) = sc.nextInt();sc.nextLine()

  /*  input処理終了  */

  // ----------------------------------------------------------


  /* process */
  // output variables
  val ans = Array("", "")
  var use_skill: Option[String] = None
  // accel, stone, thunder, avatar, cutting --  _m _o
  val skill_sub = new Array[Int](2)  // 忍術出力の補助
                                     // (sub(0), sub(1)) = (y, x)
                                     // sub(0) = 忍者id


  // 地図複製
  val allchizu = functions.copy_chizu(chizu)
  // 忍者ソウルと忍犬書き込み(忍犬優先)
  def writeAllChizu(): Unit = {
    // 忍者書き込み
    for(xy <- ninjas)
      allchizu(xy._2)(xy._1) = 'N'
    // 忍犬書き込み
    for(xy <- dogs)
      allchizu(xy._2)(xy._1) = 'D'
    // 忍者ソウル書き込み
    for(xy <- souls){
      val tmp = allchizu(xy._2)(xy._1)
      if(tmp == 'D')
        allchizu(xy._2)(xy._1) = 'd'
      else if(tmp == 'O')
        allchizu(xy._2)(xy._1) = 'o'
      else
        allchizu(xy._2)(xy._1) = 'S'
    }
  }

  writeAllChizu()


}
