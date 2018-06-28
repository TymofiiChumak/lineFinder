package com.lineFinder

import java.awt.Color

class Image(val pixels: Array[Array[Int]], val height: Int, val width: Int){
    def this(height: Int, width: Int) {
        this(Array.ofDim[Int](width, height), height, width)
    }
    def getOutlineFromImage(source: Image)(begin: Int, end: Int) = {
        val delta = 128.0;
        val filterX = Array(Array(-1, -2, -1), 
                            Array(0, 0, 0),
                            Array(1, 2, 1))
        val filterY = Array(Array(-1, 0, 1), 
                            Array(-2, 0, 2),
                            Array(-1, 0, 1))
        val b = if (begin == 0) 1 else begin
        val e = if (end == (source.width)) end - 1 else end
        for (x <- b until e; y <- 1 until (height - 1)){
            var gradX = 0;
            var gradY = 0;
            for (i <- -1 to 1; j <- -1 to 1){
                gradX += source.pixels(x + i)(y + j) * filterX(1 + i)(1 + j)
                gradY += source.pixels(x + i)(y + j) * filterY(1 + i)(1 + j)
                if (math.sqrt(gradX * gradX + gradY * gradY) > delta){
                    pixels(x)(y) = 1
                }else{
                    pixels(x)(y) = 0
                }
            }
        }
    }

    def getMonoFromImage(source: Image)(begin: Int, end: Int){
        for (i <- 0 until source.width; j <- 0 until source.height){
            val color = new Color(source.pixels(i)(j))
            pixels(i)(j) = (color.getRed() + color.getGreen() + color.getBlue()) / 3
        }
    }
}