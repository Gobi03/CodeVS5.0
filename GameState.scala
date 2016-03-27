import functions._
import debugs._

import java.util.Scanner

/*  ゲームの状態  */
class GameState(sc: java.util.Scanner) {
  val tmp = sc.nextInt()  // rest time
  sc.nextInt()  // ignore


  /*  values  */
  val time = tmp  // rest time
  // skill costs
  val costs = Map( "accel"->sc.nextInt(),
    "stone_m"->sc.nextInt(), "stone_o"->sc.nextInt(),
    "thunder_m"->sc.nextInt(), "thunder_o"->sc.nextInt(),
    "avatar_m"->sc.nextInt(), "avatar_o"->sc.nextInt(),
    "cutting"->sc.nextInt() )


  sc.nextLine()

  /* end of input */


}
