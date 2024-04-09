import ox.scl._

object Practical{

def comparator(in0: ??[Int], in1: ??[Int], out0: !![Int], out1: !![Int]): ThreadGroup= thread{
   var otherInportClosed= false
   var stop= false
    serve (

         in0 =?=> { x=>  val val1=x
                        val val2= in1?()
                        if (val1>=val2){
                                    run( thread{out0!val2} || thread{out1!val1} )
                                   
                                }
                        else {
                                    run( thread{out0!val1} || thread{out1!val2} )

                  
                                }

  
                        

                        }
                        |
        in1 =?=> { x=>  val val1=x
                        val val2= in0?()
                        if (val1>=val2){
                                    run( thread{out0!val2} || thread{out1!val1} )
       
                                }
                        else {
                                    run( thread{out0!val1} || thread{out1!val2} )
                            
                                }


                        }




    )
    out0.endOfStream; out1.endOfStream
}

def doTestComparator (in0: SyncChan[Int], in1: SyncChan[Int], out0: SyncChan[Int], out1: SyncChan[Int]): ThreadGroup = thread{
    val N=2
    val Max=100
    val xs = Array.fill(N)(scala.util.Random.nextInt(Max))
    //println(xs(0))
    //println(xs(1))
    val ys = new Array[Int](N)
    def sender = thread("sender"){ in0!xs(0); in1!xs(1); in1.endOfStream; in0.endOfStream }
    def receiver = thread("receiver"){
        repeat{ 
        ys(0) = out0?(); 
        //println(ys(0))
        ys(1) = out1?();
        //println(ys(1))
}
    
    }
    run(sender || comparator(in0, in1, out0, out1) || receiver)
    //println(ys(0))
    //println(ys(1))
    assert(xs.sorted.sameElements(ys))
}


def sort4(ins: List[SyncChan[Int]], outs: List [SyncChan[ Int ]]): ThreadGroup = thread{
require(ins.length == 4 && outs.length == 4)
/*
for i <- 0 until 4{
    xs(i)= ins(i)?()
}

val in0,in1,out0,out1= new SyncChan[Int]
*/
val ch0, ch1,ch2,ch3,ch11,c22= new SyncChan[Int]

run (comparator(ins(0),ins(2),ch0,ch2) || 
    comparator(ins(1),ins(3),ch1,ch3) || 
     comparator(ch0,ch1,outs(0),ch11) || 
     comparator(ch2,ch3,c22,outs(3)) || 
     comparator(ch11,c22,outs(1),outs(2)))
/*
def sender = thread("sender"){ in0!xs(0); in1!xs(1); in0.endOfStream; in1.endOfStream }

def receiver = thread("receiver"){
        repeat{ 
        ys(0) = out0?(); 
        println(ys(0))
        ys(1) = out1?();
        println(ys(1))
}





}
*/

}



def doTestSort4(): ThreadGroup = thread{
    val N=4
    val Max=100
    val xs = Array.fill(N)(scala.util.Random.nextInt(Max))
    val ys = new Array[Int](N)
    //val ins= new List[SyncChan[Int]]
    //val outs= new List[SyncChan[Int]]
    val ins= List.fill(N)(new SyncChan[Int])
    val outs= List.fill(N)(new SyncChan[Int])
    /*
    for (i <- 0 until N){
        ins =  
        outs=  new SyncChan[Int] :: outs 

    }
    */

    def sender = thread("sender"){ 
        for (i <- 0 until N){
            ins(i)!xs(i)
            ins(i).endOfStream
        }
    }

    def receiver = thread("receiver"){
        serve(
            outs(0) =?=> { x=> ys(0)=x} |
            outs(1) =?=> { x=> ys(1)=x} |
            outs(2) =?=> { x=> ys(2)=x} |
            outs(3) =?=> { x=> ys(3)=x} 
        )


    }  
    run(sender || sort4(ins,outs) || receiver)
    for (i <- 0 until 4){
        println("x: ")
        println(xs(i))
        println("y:")
        println(ys(i))

    }
    assert(xs.sorted.sameElements(ys))
    println(xs.sorted.sameElements(ys))
 
    }

def insertHelper(n: Int, ins: List[??[Int ]], in: ??[Int], outs: List [!![ Int ]]):ThreadGroup  = thread { 
    require(n >= 0 && n ==ins.length && outs.length == n+1)
    
    if (n==0){
        val x= in?()
        outs(0)!x
    }
    else{
        val ch1 = new SyncChan[Int]
        run (comparator(in,ins(0),outs(0),ch1) || insertHelper(n-1, ins.tail, ch1,outs.tail) )
    }

}
def insert(ins: List[??[Int ]], in: ??[Int], outs: List [!![ Int ]]): ThreadGroup = { 
    val n = ins.length; require(n >= 0 && outs.length == n+1)
    insertHelper(n,ins,in,outs)
}
/*
def binaryInsertHelper(n: Int, ins: List[??[Int ]], in: ??[Int], outs: List [!![ Int ]]):ThreadGroup  = thread { 
    require(n >= 0 && n ==ins.length && outs.length == n+1)
    val x= in?()
    val ch= new BuffChan[Int](1)
    println("x:")
    println(x)
    println("n:")
    println(n)
    if (n==0){
        outs(0)!x
        println("outs0 bitti")
        outs(0).endOfStream
        ch.endOfStream
    }
    else{
        val middle= ins(n/2)?()
        if (x<middle){
            run( binaryInsertHelper(n/2,ins.slice(0,n/2),ch,outs.slice(0,n/2+1)) || thread{ for (i <-n/2 until n) {outs(i+1)!(ins(i)?()); outs(i+1).endOfStream; print("outs bitti");println(i+1) }}    || thread{ch!x; ch.endOfStream})  
        }
        else{
            run( binaryInsertHelper(n-n/2-1,ins.slice(n/2+1,n),ch,outs.slice(n/2+1,n+1)) || thread{ for (i <-0 until (n/2+1)) {outs(i+1)!(ins(i)?()); outs(i+1).endOfStream ; print("outs bitti");println(i+1)}} || thread{ch!x ; ch.endOfStream}  )  
        }
        for (i <- 0 until (n+1)){
            outs(i).endOfStream

        }
    }
}
*/

def binaryInsertHelper(n: Int, ins: List[??[Int ]], in: ??[Int], outs: List [!![ Int ]]):ThreadGroup  = thread { 
    require(n >= 0 && n ==ins.length && outs.length == n+1)
    
    if (n==0){
        val x= in?()
        outs(0)!x
        outs(0).endOfStream
    }
    else{
        val ch1,ch2= new BuffChan[Int](1)
        run(comparator(in,ins(n/2),ch1,ch2) ||
         binaryInsertHelper(n/2,ins.slice(0,n/2),ch1,outs.slice(0,n/2+1)) ||  
         binaryInsertHelper(n-n/2-1,ins.slice(n/2+1,n),ch2,outs.slice(n/2+1,n+1)))
    }
    




}


def binaryInsert(ins: List[??[Int ]], in: ??[Int], outs: List [!![ Int ]]): ThreadGroup = { 
    val n = ins.length; require(n >= 0 && outs.length == n+1)
    binaryInsertHelper(n,ins,in,outs) 
}



def insertionSortHelper(n: Int, ins: List[??[Int ]], outs: List [!![ Int ]]): ThreadGroup = thread {
    require(n >= 2 && n == ins.length && outs.length == n) 
    val channels= List.fill(n-1)(new SyncChan[Int])
    val nul= List.fill(0)(new SyncChan[Int])
    if (n==2){
        //insert(List(ins(1)),ins(0),outs)
       //val x= ins(0)?()
       //outs(0)!x 
       run ( comparator(ins(0),ins(1),outs(0),outs(1)))
    }
    else{
     run (insert(channels,ins(0),outs) || insertionSortHelper(n-1,ins.tail,channels))
    }
    for (i <- 0 until (n-1)){
        channels(i).endOfStream
    }
    for (i <- 0 until (n)){
        outs(i).endOfStream
    }


}


def insertionSort(ins: List[??[Int ]], outs: List [!![ Int ]]): ThreadGroup =  {
val n = ins.length; require(n >= 2 && outs.length == n) 
insertionSortHelper(n,ins,outs)
}


def doTestInsert(): ThreadGroup= thread{
    val N=6
    val Max=100
    val xs = Array.fill(N)(scala.util.Random.nextInt(Max))
    val SortedXs= xs.sorted
    val ys = new Array[Int](N+1)
    val in = new SyncChan[Int]
    val insertedInt= scala.util.Random.nextInt(Max)
    
    val ins= List.fill(N)(new SyncChan[Int])
    val outs= List.fill(N+1)(new SyncChan[Int])

    def sender = thread("sender"){ 
        for (i <- 0 until N){
            ins(i)!SortedXs(i)
            ins(i).endOfStream
        }
        in!insertedInt
        in.endOfStream
    }

    def receiver = thread("receiver"){
        for (i <- 0 until (N+1)){
            ys(i)= outs(i)?()
            print("ys "); print(i); print(":"); println(ys(i))
        /*
        serve(
            outs(0) =?=> { x=> ys(0)=x} |
            outs(1) =?=> { x=> ys(1)=x} |
            outs(2) =?=> { x=> ys(2)=x} |
            outs(3) =?=> { x=> ys(3)=x} 
        )
        */
        }
    }  
    run(sender || insert(ins,in, outs) || receiver)
    println("x")
    println(insertedInt)
    println("xs")
    for(i <- 0 until(N)){
        println(SortedXs(i))
    }
    println("ys")
    for(i <- 0 until(N+1)){
        println(ys(i))
    }

    }

def doTestBinaryInsert(): ThreadGroup= thread{
    val N=8
    val Max=100
    val xs = Array.fill(N)(scala.util.Random.nextInt(Max))
    val SortedXs= xs.sorted
    val ys = new Array[Int](N+1)
    val in = new BuffChan[Int](1)
    val insertedInt= scala.util.Random.nextInt(Max)
    
    val ins= List.fill(N)(new BuffChan[Int](1))
    val outs= List.fill(N+1)(new BuffChan[Int](1))

    def sender = thread("sender"){ 
        for (i <- 0 until N){
            ins(i)!SortedXs(i)
            ins(i).endOfStream
        }
        in!insertedInt
        in.endOfStream
    }

    def receiverUnit(i:Int) = thread("worker"){
        ys(i)= outs(i)?()
  }

    def receiver = {
        val workers = || (for (i <- 0 until (N+1)) yield receiverUnit(i))
        workers 
    }
    /*
    def receiverUnit(i:Int) = thread{
        ys(i)= outs(i)?()
    }
    */
    /*
    def receiver = {
        val receivers = || (for (i <− 0 until (N+1)) yield  receiverUnit(i)) 
        receivers
  }  
  */

    /* def receiver = thread("receiver"){
        for (i <- 0 until (N+1)){
            ys(i)= outs(i)?()
            println("y:")
            println(ys(i))
             }
    */

        /*
        serve(
            outs(0) =?=> { x=> ys(0)=x} |
            outs(1) =?=> { x=> ys(1)=x} |
            outs(2) =?=> { x=> ys(2)=x} |
            outs(3) =?=> { x=> ys(3)=x} 
        )
        */
       
  
    run(sender || binaryInsert(ins,in, outs) || receiver)
    println("x")
    println(insertedInt)
    println("xs")
    for(i <- 0 until(N)){
        println(SortedXs(i))
    }
    println("ys")
    for(i <- 0 until(N+1)){
        println(ys(i))
    }

    }






def doTestInsertionSort():ThreadGroup= thread{
    val N=10
    val Max=100
    val xs = Array.fill(N)(scala.util.Random.nextInt(Max))
    val ys = new Array[Int](N)

    val ins= List.fill(N)(new SyncChan[Int])
    val outs= List.fill(N)(new SyncChan[Int])

     def sender = thread("sender"){ 
        for (i <- 0 until N){
            ins(i)!xs(i)
            ins(i).endOfStream
        }
    }

    def receiver= thread("receiver"){
        for (i <- 0 until (N)){
            ys(i)= outs(i)?()
            outs(i).endOfStream
        }
    }


    run(sender || insertionSort(ins, outs) || receiver)

    println("xs")
    for(i <- 0 until N ){
        println(xs(i))
    }
    println("ys")
    for(i <- 0 until N){
        println(ys(i))
    }

    assert(xs.sorted.sameElements(ys))
    println(xs.sorted.sameElements(ys))




}











  def main(args : Array[String]) = {
    val in0, in1, out0, out1= new SyncChan[Int]
    println("Comparator test")
    run(doTestComparator(in0, in1, out0, out1))
    println(" test sort 4")
    run(doTestSort4())
    println("test insert")
    run(doTestInsert())
    println("test insertion sort")

    run (doTestInsertionSort())
    println("binary insert")

    run(doTestBinaryInsert())
  }


}