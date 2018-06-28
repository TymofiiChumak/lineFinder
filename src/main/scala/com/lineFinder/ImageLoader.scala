package com.lineFinder

import javax.imageio.ImageIO
import java.io.File

object  ImageLoader {

    def loadImage(path: String) = {
        println(path)
        val file = new File(path)
        val image = ImageIO.read(file)
        val width = image.getWidth()
        val height = image.getHeight()
        println(height, width)
        val pixels = Array.ofDim[Int](width, height)
        for (i <- 0 until width; j <- 0 until height){
            pixels(i)(j) = image.getRGB(i,j)
        }
        new Image(pixels, height, width)
    }

}