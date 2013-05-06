import java.util.ArrayList
import Math.pow
import scala.util.Sorting.quickSort

class scala_iter[A](arr: ArrayList[Object])extends Iterator[A]{
	var stored = arr;
	var index = 0;

	def hasNext():Boolean={
		if (this.index < this.stored.size()){
			return true
		}else{ return false}
	}

	def next(): A={
		this.index +=1
		return (this.stored.get(this.index-1)).asInstanceOf[A]
	}
}

class scala_arr[A](arr: ArrayList[Object]) extends Seq[A]{	
	var stored = arr;

	def apply(idx:Int):A ={
		return this.stored.get(idx).asInstanceOf[A]
	}

	def iterator():Iterator[A]={
		return new scala_iter[A](this.stored)
	}

	def length():Int = {
		return this.stored.size()
	}
}

object scala_lib{

    def dot(arr1: Array[Double], arr2:Array[Double]):Double= {
        var out = 0.0
        var i =0
        while (i < arr1.length){
            out += arr1(i) * arr2(i)
            i+=1
        }
        return out
    }

	def zip[T](list1: scala.collection.mutable.MutableList[T], list2: scala.collection.mutable.MutableList[T]): 
            scala.collection.mutable.MutableList[scala.collection.mutable.MutableList[T]] ={
        list1.zip(list2).map(tup => (scala.collection.mutable.MutableList[T]()++(tup.productIterator)).asInstanceOf[scala.collection.mutable.MutableList[T]] )
    }

    def zip[T: Manifest] (arr1: Array[T], arr2: Array[T]): Array[Array[T]]= {
    	arr1.zip(arr2).map( tup => {
    		var out = new Array[T](2)	
    		out(0) =tup._1.asInstanceOf[T]
    		out(1) =tup._2.asInstanceOf[T]
    		out
    	})
    }

    def copy_n[T](list: scala.collection.mutable.MutableList[T], copies: Int): scala.collection.mutable.MutableList[T] ={
    	var out = new scala.collection.mutable.MutableList[T]()
    	for (i <- Range(0,copies)){
    		out ++= list.clone
    	}
    	return out
    }

    //should make this recursive in case T is also a nested array
    def duplicate_arr[T: Manifest](arr:Array[T]): Array[T] ={
        var out_arr = new Array[T](arr.size)
        Array.copy(arr, 0, out_arr, 0, arr.size)
        return out_arr
    }

    def copy_n[T : Manifest](arr: Array[T], copies: Int): Array[T] ={
    	var out = new Array[T](arr.size * copies)
    	for (i <- Range(0,copies)){
    		for (j <- Range(0, arr.size)){
                out(i*arr.size+j) = arr(j)
    		}
    	}
    	return out
    }

    def copy_n[T : Manifest](arr: Array[Array[T]], copies: Int): Array[Array[T]] ={
        var out = new Array[Array[T]](arr.size * copies)
        for (i <- Range(0,copies)){
            for (j <- Range(0, arr.size)){
                out(i*arr.size+j) = duplicate_arr(arr(j))
            }
        }
        return out
    }

    def sort(arr: Array[Double]) {
        quickSort(arr)
    }

    def sort(arr: Array[Float]) {
        quickSort(arr)
    }

    def sort(arr: Array[Int]) {
        quickSort(arr)
    }

    def sort(arr:Array[Array[Double]]){
        quickSort(arr)(Ordering.by[Array[Double], Double](_.apply(0)))
    }

    def sort(arr:Array[Array[Float]]){
        quickSort(arr)(Ordering.by[Array[Float], Float](_.apply(0)))
    }

    def sort(arr:Array[Array[Int]]){
        quickSort(arr)(Ordering.by[Array[Int], Int](_.apply(0)))
    }

    def convert_to_int(number:String):Int ={
    	return Integer.parseInt(number)
    }

    def convert_to_int(number:Double):Int ={
    	return number.asInstanceOf[Int]
    }

    def subscript_get[T: Manifest](arr: Array[T], index:Int): T ={
        return arr(index)
    }

    def subscript_set[T: Manifest](arr: Array[T], index:Int, item:T) {
        arr(index) = item
    }

    def subscript_get(tup: Product, index:Int): Any={
        return tup.productElement(index)    
    }

    def mean(arr: Array[Double]): Double={
        return arr.reduce(_+_) / arr.length
    }    

    def mean(arr: Array[Float]): Float={
        return arr.reduce(_+_) / arr.length
    }

    def mean(list: scala.collection.mutable.MutableList[Double]): Double={
    	return list.reduce(_+_) / list.length
    }

    def std_dev(arr: Array[Double]): Double ={
        var mean = arr.reduce(_+_) / arr.length
        var squared_dif = 0.0
        arr.foreach{ elem => squared_dif += (mean - elem) * (mean - elem)}
        return Math.pow((squared_dif / (arr.length-1)), .5)
    }

    def std_dev(arr: Array[Float]): Float ={
        var mean = arr.reduce(_+_) / arr.length
        var squared_dif = 0.0
        arr.foreach{ elem => squared_dif += (mean - elem) * (mean - elem)}
        return Math.pow((squared_dif / (arr.length-1)), .5).asInstanceOf[Float]
    }

    def std_dev(list: scala.collection.mutable.MutableList[Double]): Double ={
    	var mean = list.reduce(_+_) / list.length
    	var squared_dif = 0.0
    	list.foreach{ elem => squared_dif += (mean - elem) * (mean - elem)}
    	return Math.pow((squared_dif / (list.length-1)), .5)
    }

    //hand code in variance too ?
    //population standard deviation ?
}









