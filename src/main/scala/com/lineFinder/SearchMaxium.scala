package com.lineFinder

class SearchMaxium(val matrix: Array[Array[Int]], val width: Int, val height: Int, val min: Int) extends Iterable[(Int, Int)]{
    val isMaxium = Array.ofDim[Boolean](width, height)    
    def setBeginValues {
        for (i <- 0 until width; j <- 0 until height){
            if (matrix(i)(j) < min) {
                isMaxium(i)(j) = false
            }else{
                isMaxium(i)(j) = true
            }
        }
    }
    def process(begin: Int, end: Int) {
        setBeginValues
        def cutLess(i: Int, j: Int){
            for (l <- -1 to 1; k <-1 to 1){
                if ( l != k && (i + l) >= 0 && (i + l) < width && (j + k) >= 0 && (j + k) < height){
                    if (isMaxium(i + l)(j + k) && matrix(i)(j) >= matrix(i + l)(j + k)){
                        isMaxium(i + l)(j + k) = false
                        cutLess(i + l, j + k)
                    }
                }
            }
        }
        for (i <- begin until end; j <- 0 until height){
            if (isMaxium(i)(j)){
                for (l <- -1 to 1; k <-1 to 1){
                    if (l != k && (i + l) >= 0 && (i + l) < width && (j + k) >= 0 && (j + k) < height){
                        if (isMaxium(i + l)(j + k) && matrix(i)(j) > matrix(i + l)(j + k)){
                            cutLess(i + l, j + k)
                        }
                    }
                }
            }
        }
    }
    class SMIterator extends Iterator[(Int, Int)]{
        var i = 0
        var j = 0
        var end = false
        override def next() = {
            def getNext() {
                j += 1
                if (j >= height) {
                    j = 0 
                    i += 1
                    if (i == width){
                        end = true
                    }
                }
            }
            while (!end && !isMaxium(i)(j)){
                getNext()
            }
            val maxium = (i, j)
            getNext()
            maxium
        }
        override def hasNext = !end
    }

    override def iterator = new SMIterator

    def getMaxiums() = {
        var maxiums = Array[(Int, Int)]()
        for (i <- 0 until width; j <- 0 until height){
            if (isMaxium(i)(j)) maxiums :+ (i, j)
        }
    }
}