/*
function a() { 
 var i = 0; 
 function b() { console.log(++i); } 
 return b;
 }

var c = a();
c();
*/

/* 我们通过subFuncs返回函数数组，然后分别调用执行 */

// 返回函数的数组subFuncs，而这些函数对superFunc的变量有引用
// 这就是一个典型的闭包
// 那么有什么问题呢？
// 当我们回头执行subFuncs中的函数的时候，我们得到的i其实一直都是10，为什么？
// 因为当我们返回subFuncs之后，superFunc中的i=10
// 所以当执行subFuncs中的函数的时候，输出i都为10。
// 
// 以上，就是闭包最大的坑，一句话理解就是：
// 子函数对父函数变量的引用，是父函数运行结束之后的变量的状态
function superFunc1() {
    var subFuncs = new Array();
    for (var i = 0; i < 5; i++) {
        subFuncs[i] = function() {
            return i;
        };
    }
    

    return subFuncs;
}

// 那么，如何解决上诉的闭包坑呢？
// 其实原理很简单，既然闭包坑的本质是：子函数对父函数变量的引用，是父函数运行结束之后的变量的状态
// 那么我们解决这个问题的方式就是：子函数对父函数变量的引用，使用运行时的状态
// 如何做呢？
// 在函数表达式的基础上，加上自执行即可。
function superFunc2() {
    var subFuncs = new Array();
    for (var i = 0; i < 5; i++) {
        subFuncs[i] = function(num) {
            return function() {
                return num;
            };
        }(i);
    }
    return subFuncs;
}




var result = superFunc1();
for (var j=0;j<5;j++)
    console.log(result[j]());
