// 父类
function SuperType() {
    this.property = true;
}
SuperType.prototype.getSuperValue = function() {
    return this.property;
};

// 子类
function SubType() {
    this.subproperty = false;
}

//子类继承父类
SubType.prototype = new SuperType();

//给子类添加新方法
SubType.prototype.getSubValue = function() {
    return this.subproperty;
};
//重写父类的方法
SubType.prototype.getSuperValue = function() {
    return false;
};

// 实例化
var instance = new SubType();
console.log(instance.getSuperValue()); //输出false