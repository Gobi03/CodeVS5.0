import functions._
import debugs._

class Attack(enemy: PlayerState, own: PlayerState){
  val h = 17; val w = 14

  /* dogchizu, dogcomeの設定 書き込みまでする */
  //--------------------------------------------------------//
  // DW_O の情報だけ書いた地図作成
  def makeDogChizu(): Array[Array[Char]] = {
    val dogchizu = copy_chizu(own.chizu)

    for(xy <- own.dogs)
      dogchizu(xy._2)(xy._1) = 'D'

    return dogchizu
  }

  // 忍犬の進行可能エリアを記入
  def setDogCome(dogCome: Array[Array[Boolean]]): Unit = {
    for(xy <- own.dogs){
      val x = xy._1; val y = xy._2
      dogCome(y)(x) = true

      val tmpl = uldrList(x, y)
      for(p <- tmpl){
        val ne_x = p._1._1; val ne_y = p._1._2
        dogCome(ne_y)(ne_x) = true
      }
    }
  }


  // DW_O の地図
  val dogchizu: Array[Array[Char]] = this.makeDogChizu()
  // dogが要るor来る場所をtrueに
  val dogCome = Array.ofDim[Boolean](h, w)
  this.setDogCome(dogCome)
  //--------------------------------------------------------//



  // id番の忍者がこのターン静止しても生きられるか
  // true:生きる　　false:死ぬ
  def isIdSafe(id: Int): Boolean = {
    val xy = enemy.ninjas(id)
    return !isDogWillCome(xy._1, xy._2, enemy.allchizu)
  }

  // id番の忍者が次に動ける方向はいくつあるか return.length
  //  'U', 'D', 'R', 'L'から移動可能地点を返す
  def whereCanNinjaGo(id: Int): List[Char] = {
    var res: List[Char] = Nil

    val xy = enemy.ninjas(id)
    val nlist: List[((Int, Int), Char)] = uldrList(xy._1, xy._2)

    for(info <- nlist){
      val nx = info._1._1; val ny = info._1._2
      dogchizu(ny)(nx) match{
        case 'D' | 'W' => ()
        case '_' =>  // 犬来てかつそこから上下左右行けないなら落石
          if(dogCome(ny)(nx)){
            var flag = true  // 移動可能だったら折る
            val udlr = List('U','D','L','R')
            for(e <- udlr){
              val last: (Int, Int) = moving(info._1, e)
              if(dogCome(last._2)(last._1)) ()
              else{ // 犬来ない
                dogchizu(last._2)(last._1) match{
                  case '_' => flag = false
                  case 'O' => // 押せるかどうか
                    val tmp: (Int, Int) = moving(last, e)
                    dogchizu(tmp._2)(tmp._1) match{
                      case '_' => flag = true
                      case  _  => ()  // DWO
                    }
                  case  _  => ()  // DW
                }
              }
            }
            if(flag) ()
            else res = info._2 :: res
          }
          else
            res = info._2 :: res
        case 'O' => // 次点が 岩or犬or壁 なら移動不可
          val nex_xy = moving(info._1, info._2)
          val nextc = dogchizu(nex_xy._2)(nex_xy._1)
          nextc match{
            case '_' => res = info._2 :: res
            case  _  => ()
          }
        case  _  => ()  // error
      }
    }

    return res
  }


  private def setStoneE(xy: (Int, Int)): Unit = {
    own.use_skill = Some("stone_o")
    own.skill_sub(0) = xy._1
    own.skill_sub(0) = xy._2
  }

  //-----------------------------------------------------
  /* process */
  // uldrListの出力 (List[((Int, Int), Char)]) 受け取って
  // 次ターン犬が来る可能性があるときに落石で逃げ場塞げるなら塞ぐ
  // DW_O
  def process(): Unit = {
    for(id <- 0 to 1){
      if(!isIdSafe(id)){  // 動かなきゃ死ぬ
        val nextClist: List[Char] = whereCanNinjaGo(id)
        if(nextClist.length == 1){  // 移動可能経路がひとつのみの場合
          val vec: Char = nextClist.head

          val nex: (Int, Int) = functions.moving(enemy.ninjas(id), vec)
          val nexnex: (Int, Int) = functions.moving(nex, vec)

          dogchizu(nex._2)(nex._1) match{
            case '_' =>
              dogchizu(nexnex._2)(nexnex._1) match{
                case '_' => ()
                case  _  => // DWO
                  this.setStoneE(nex)
              }
            case 'O' =>
              dogchizu(nexnex._2)(nexnex._1) match{
                case '_' => // 落石
                  this.setStoneE(nexnex)
                case  _  => ()
              }
            case _  => ()  // error
          }
        }
      }
    }
  }
}
