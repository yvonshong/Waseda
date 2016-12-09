//introduction
/*
* Programming language
* SONG DAIWEI
*/ 

var intNumber_a = 5;
var doubleNumber_a = intNumber_a/3;

//console.log(somethingIDidntDefined);

if(doubleNumber_a>0)
    console.log("number: " + doubleNumber_a);
else
    console.log("doubleNumber_a is less than 0");
console.log("=======switch==========")
for(var i=0;i<4;i++)
    switch(i)
    {
        case 1:
            console.log("i=1");
            break;
        case 2:
            console.log("i=2");
            break;
        default:
            console.log(i);
            break;
    }
    
console.log("=======while==========")
while(intNumber_a>0)
{
    console.log(intNumber_a);
    intNumber_a--;
}
console.log("======for_in===========")
var testArray=[3,"2th","001"];
for (item in testArray)
    console.log(testArray[item]);

console.log("======function===========")

function fabonacci(n) {
    //variable = "global variable";
    return n<2 ?  n :  fabonacci(n - 1) + fabonacci(n - 2);
}
for(var j = 0;j<6;j++)
    console.log(fabonacci(j));

//console.log(variable);



