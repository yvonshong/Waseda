function outputNumbers(count) {
    // 在函数作用域下，利用闭包封装块级作用域
    // 这样的话，i在外部不可用，便有了类似块级作用域
    (function() {
        for (var i = 0; i < count; i++) {
            alert(i);
        }
    })();

    alert(i); //导致一个错误! 
}

// 在全局作用域下，利用闭包封装块级作用域
// 这样的话，代码块不会对全局作用域造成污染
(function() {
    var now = new Date();

    if (now.getMonth() == 0 && now.getDate() == 1) {
        alert("Happy new year!");
    }
})();

// 是的，封装块级作用域的核心就是这个：函数表达式 + 自执行！
(function() {
    //这里是块级作用域
})();



(function() {
    //私有变量和私有函数
    var privateVariable = 10;

    function privateFunction() {
        return false;
    }

    //构造函数
    MyObject = function() {};
    //公有/特权方法
    MyObject.prototype.publicMethod = function() {
        privateVariable++;

        return privateFunction();
    };
})();