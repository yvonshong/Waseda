var seq2 = "GACTACGATCCGTATACGCACAGGTTCAGAC";
var seq1 = "GACTACAGCTCGTATACGCACACATGGTTCAGAC";
var gap_opening_penalty=-2;
var gap_extension_penalty=-6;
var matchMatrix=[
    [2,-5,-7,-7],
    [-5,2,-7,-7],
    [-7,-7,2,-5],
    [-7,-7,-5,2]
    ];

var matrix= new Array(seq1.length+1);
for(var p=0;p<seq1.length+1;p++)
{
    matrix[p]=new Array(seq2.length+1);
    for(var q=0;q<seq2.length+1;q++)
    {
        matrix[p][q]={value:null,last:null,flag:null};
    }
}

// find the value
for(var i=0;i<seq1.length+1;i++)
{
    for(var j=0;j<seq2.length+1;j++)
    {
        var result=value(i,j);
        
        matrix[i][j]={
            value:result.value,
            last:result.last,
            flag:result.flag
        }
    }
}


function value(i,j)
{
    //initial
    if(i==0&&j==0)
    {
        return {value:0,last:null,flag:null};
    }
    var value=0;
    var max=nearMax(i,j);
    
    matrix[i][j].last=max.position;
    
    if(max.position[0]==i-1 && max.position[1]==j-1)
    {    
         value=max.value+match(i,j);
         return {value:value,last:max.position,flag:"match"};
    }
    else{
        value=max.value+gap(i,j);
         return {value:value,last:max.position,flag:"gap"};
    }
    
}



//find the max value and its location near the grid
function nearMax(m,n)
{
    var max=0;
    var value=0;
    var last=null;
    if(m==0&&n>0)
        return {value:matrix[0][n-1].value,position:[0,n-1]};
    if(m>0&&n==0)
        return {value:matrix[m-1][0].value,position:[m-1,0]};
    if(m*n>0)
    {
        max=Math.max(matrix[m-1][n-1].value,matrix[m][n-1].value,matrix[m-1][n].value);
        if(max==matrix[m-1][n-1].value)
            return {value:matrix[m-1][n-1].value,position:[m-1,n-1]};
        if(max==matrix[m][n-1].value)
            return {value:matrix[m][n-1].value,position:[m,n-1]};
        if(max==matrix[m-1][n].value)
            return {value:matrix[m-1][n].value,position:[m-1,n]};
    }
}

function gap(i,j)
{
    var lp=matrix[i][j].last;
    if(matrix[lp[0]][lp[1]].flag=="gap")
        return gap_extension_penalty;
    else
        return gap_opening_penalty;
}

function match(i,j)
{   
    function AGTC(str)
    {
        if(str=="A")
            return 0;
        if(str=="G")
            return 1;
        if(str=="T")
            return 2;
        if(str=="C")
            return 3;
    };
    return matchMatrix[AGTC(seq1.charAt(i-1))][AGTC(seq2.charAt(j-1))];
}

var output= new Array(seq1.length+1);
for(var p1=0;p1<seq1.length+1;p1++)
{
    output[p1]=new Array(seq2.length+1);
}
//output
for(var i0=0;i0<seq1.length+1;i0++)
{
    for(var j0=0;j0<seq2.length+1;j0++)
    {
        var t=matrix[i0][j0].value;
        if(t >= 10)
            output[i0][j0]=" "+t;
        if(t >= 0 && t < 10)
            output[i0][j0]="  "+t;
        if(t > -10 && t < 0)
            output[i0][j0]=" "+t;
        if(t <= -10)
            output[i0][j0]=t;
    }
}
//output the matrix
for(var i1=0;i1<seq1.length+1;i1++)
{
   var row=output[i1].join(" ");
   console.log(row);
}
//TO-DO
//output the path


