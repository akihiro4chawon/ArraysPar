ArraysPar
=========

ArraysPar is a parallel implementation of java.util.Arrays.
It offers parallel sort functions, which run n-times faster than java.util.Arrays when executed on an n-core processor.


Example
-------

    import akihiro.ArraysPar
    
    val a: Array[Int] = util.Random.shuffle(Seq.range(1, 1000000)).toArray
    
    ArraysPar.sort(a)


