import scala.collection.mutable.PriorityQueue

trait Heap 

case class BPQ(x:Int, pq:PriorityQueue[BPQ]) extends Ordered[BPQ] with  Heap{

	def compare(that:BPQ) = that.x compare this.x
}

case object Empty extends  Heap



def makeSingletonBPQ(x:Int):BPQ =  BPQ(x, new PriorityQueue())

def merge(bpq1:BPQ, bpq2:BPQ):BPQ = {
	if (bpq1.x < bpq2.x) BPQ(bpq1.x, bpq1.pq ++ Seq(bpq2))
	else  BPQ(bpq2.x, bpq2.pq ++ Seq(bpq1))
}	

def insert(bpq:BPQ, x:Int):BPQ = merge(bpq, makeSingletonBPQ(x))

def getMin(bpq:BPQ):Int = bpq.x


def extractMin(bpq:BPQ):Tuple2[Int, BPQ] = {

	val tuple  = Tuple2(bpq.pq.head, bpq.pq.tail)
	Tuple2(bpq.x, BPQ(bpq.x, tuple._1.pq ++ tuple._2))
}

def deleteMin(bpq:BPQData):BPQData = bpq match {

	case (bpq.pq == Empty) => Empty 
	case _ = Empty
}
 





val bpq  = (1 to 100).toSeq.map(x => makeSingletonBPQ(x)).reduce((x, y) => merge(x,y))

val newBpq = (101 to 200).toSeq.map(x => makeSingletonBPQ(x)).reduce((x, y) => merge(x,y))

val bpq1 = insert(bpq, 0)

val bpq2 = merge(bpq,  bpq1) 

val minElem = getMin(bpq2)

val extractedMin = extractMin(bpq2)

