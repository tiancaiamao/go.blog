    (define member
      _ [] -> false
      X [X | _] -> true
      X [_ | Y] -> (member X Y))
      
第一步，将所有链表转成cons形式，将所有通配符转成唯一的变量

    (define member
      V [] -> false
      X (cons X W) -> true
      X (cons Z Y) -> (member X Y))
      
第二步，确认所有规则都是left linear的，left linear是指箭头左边不会包含两个相同的变量。

如果不满足，可以通过一个替换实现

    (define member
      V [] -> false
      X (cons U W) -> true where (= U X)
      X (cons Z Y) -> (member X Y))
      
第三步，计算函数参数个数n，确保所有规则参数个数是一致的。

    member = (/. A (/. B 
      (V [] -> false
      X (cons U W) -> true where (= U X)
      X (cons Z Y) -> (member X Y))))

第四步是将各个独立的规则替换成一个使用模式匹配的lambda表达式。常规的lambda表达式只包含一个变量，而模式匹配lambda表达式中可以是symbol,variable,string,number,boolean,character，或者是空链表，一个tuple，或者由模式组成的cons表达式。

    (/. X X)
    (/. (cons 1 []) 2)
    
前者是一个标准的lambda表达式，而后者是模式匹配的lambda表达式。

    (/. (cons 1 []) 2)	这个函数如果接受的输入是(cons 1 [])则返回2
    (/. 1 2)			这个函数如果接受的输入是1则返回2
    (/. [] [])		 	这个函数如果接受的输入是[]则返回[]
    (/. (cons X []) X)	这个函数接受一个单元素的链表，并返回链表第一个元素
    
对于之前的例子，规则

    V [] -> false
    X (cons U W) -> true where (= U X)
    X (cons Z Y) -> (member X Y)
    
分别变为

    (/. V (/. [] false))
    (/. X (/. (cons U W) (where (= u X) true)))
    (/. X (/. (cons Z Y) (member X Y)))
    
第五步，应用函数的参数，得到case表达式

    (/. A (/. B
      (cases
        (((/. V (/. [] false)) A) B)
        (((/. X (/. (cons U W) (where (= u X) true))) A) B)
        (((/. X (/. (cons Z Y) (member X Y))) A) B))))

cases的语义是在它的作用域内依次对各个表达式求值，遇到首个成功就返回。如果所有表达式都返回失败，则返回失败，表示匹配不到。
      
最后，包装到一个Y组合子里面

    (Y (/. M (/. A (/. B
      (cases
            (((/. V (/. [] false)) A) B)
            (((/. X (/. (cons U W) (where (= u X) true))) A) B)
            (((/. X (/. (cons Z Y) (M X Y))) A) B))))))


如何处理相互递归呢？

    (define even?
       {number --> boolean}
       1 -> false
       X -> (odd? (- X 1)))
    (define odd?
       {number --> boolean}
       1 -> true
       X -> (even? (- X 1)))

先分别转化它们：

    (/. A (cases ((/. 1 false) A)
                ((/. X (odd? (- X 1))) A)))
    (/. A (cases ((/. 1 true) A)
                ((/. X (even? (- X 1))) A)))

然后放到一个tuple里面

    (@p (/. A (cases ((/. 1 false) A)
                    ((/. X (odd? (- X 1))) A)))
        (/. A (cases ((/. 1 true) A)
                    ((/. X (even? (- X 1))) A))))
                    
然后将递归函数调用转换成tuple里面的位置

    (@p (/. A (cases ((/. 1 false) A)
                    ((/. X ((snd T) (- X 1))) A)))
        (/. A (cases ((/. 1 true) A)
                    ((/. X ((fst T) (- X 1))) A))))
                    
最后放到Y组合子里面

    (Y (/. T (@p (/. A (cases ((/. 1 false) A)
                        ((/. X ((snd T) (- X 1))) A)))
                  (/. A (cases ((/. 1 true) A)
                              ((/. X ((fst T) (- X 1))) A))))))
