var BLOSUM62 = {
    'A':{'A':  4, 'C':  0, 'B': -2, 'E': -1,
        'D': -2, 'G':  0, 'F': -2, 'I': -1, 'H': -2,
        'K': -1, 'M': -1, 'L': -1, 'N': -2, 'Q': -1,
        'P': -1, 'S':  1, 'R': -1, 'T':  0, 'W': -3,
        'V':  0, 'Y': -2, 'X':  0, 'Z': -1},

    'C':{'A':  0, 'C':  9, 'B': -3, 'E': -4,
        'D': -3, 'G': -3, 'F': -2, 'I': -1, 'H': -3,
        'K': -3, 'M': -1, 'L': -1, 'N': -3, 'Q': -3,
        'P': -3, 'S': -1, 'R': -3, 'T': -1, 'W': -2,
        'V': -1, 'Y': -2, 'X': -2, 'Z': -3},

    'B':{'A': -2, 'C': -3, 'B':  4, 'E':  1,
        'D':  4, 'G': -1, 'F': -3, 'I': -3, 'H':  0,
        'K':  0, 'M': -3, 'L': -4, 'N':  3, 'Q':  0,
        'P': -2, 'S':  0, 'R': -1, 'T': -1, 'W': -4,
        'V': -3, 'Y': -3, 'X': -1, 'Z':  1},

    'E':{'A': -1, 'C': -4, 'B':  1, 'E':  5,
        'D':  2, 'G': -2, 'F': -3, 'I': -3, 'H':  0,
        'K':  1, 'M': -2, 'L': -3, 'N':  0, 'Q':  2,
        'P': -1, 'S':  0, 'R':  0, 'T': -1, 'W': -3,
        'V': -2, 'Y': -2, 'X': -1, 'Z':  4},

    'D':{'A': -2, 'C': -3, 'B':  4, 'E':  2,
        'D':  6, 'G': -1, 'F': -3, 'I': -3, 'H': -1,
        'K': -1, 'M': -3, 'L': -4, 'N':  1, 'Q':  0,
        'P': -1, 'S':  0, 'R': -2, 'T': -1, 'W': -4,
        'V': -3, 'Y': -3, 'X': -1, 'Z':  1},

    'G':{'A':  0, 'C': -3, 'B': -1, 'E': -2,
        'D': -1, 'G':  6, 'F': -3, 'I': -4, 'H': -2,
        'K': -2, 'M': -3, 'L': -4, 'N':  0, 'Q': -2,
        'P': -2, 'S':  0, 'R': -2, 'T': -2, 'W': -2,
        'V': -3, 'Y': -3, 'X': -1, 'Z': -2},

    'F':{'A': -2, 'C': -2, 'B': -3, 'E': -3,
        'D': -3, 'G': -3, 'F':  6, 'I':  0, 'H': -1,
        'K': -3, 'M':  0, 'L':  0, 'N': -3, 'Q': -3,
        'P': -4, 'S': -2, 'R': -3, 'T': -2, 'W':  1,
        'V': -1, 'Y':  3, 'X': -1, 'Z': -3},

    'I':{'A': -1, 'C': -1, 'B': -3, 'E': -3,
        'D': -3, 'G': -4, 'F':  0, 'I':  4, 'H': -3,
        'K': -3, 'M':  1, 'L':  2, 'N': -3, 'Q': -3,
        'P': -3, 'S': -2, 'R': -3, 'T': -1, 'W': -3,
        'V':  3, 'Y': -1, 'X': -1, 'Z': -3},

    'H':{'A': -2, 'C': -3, 'B':  0, 'E':  0,
        'D': -1, 'G': -2, 'F': -1, 'I': -3, 'H':  8,
        'K': -1, 'M': -2, 'L': -3, 'N':  1, 'Q':  0,
        'P': -2, 'S': -1, 'R':  0, 'T': -2, 'W': -2,
        'V': -3, 'Y':  2, 'X': -1, 'Z':  0},

    'K':{'A': -1, 'C': -3, 'B':  0, 'E':  1,
        'D': -1, 'G': -2, 'F': -3, 'I': -3, 'H': -1,
        'K':  5, 'M': -1, 'L': -2, 'N':  0, 'Q':  1,
        'P': -1, 'S':  0, 'R':  2, 'T': -1, 'W': -3,
        'V': -2, 'Y': -2, 'X': -1, 'Z':  1},

    'M':{'A': -1, 'C': -1, 'B': -3, 'E': -2,
        'D': -3, 'G': -3, 'F':  0, 'I':  1, 'H': -2,
        'K': -1, 'M':  5, 'L':  2, 'N': -2, 'Q':  0,
        'P': -2, 'S': -1, 'R': -1, 'T': -1, 'W': -1,
        'V':  1, 'Y': -1, 'X': -1, 'Z': -1},

    'L':{'A': -1, 'C': -1, 'B': -4, 'E': -3,
        'D': -4, 'G': -4, 'F':  0, 'I':  2, 'H': -3,
        'K': -2, 'M':  2, 'L':  4, 'N': -3, 'Q': -2,
        'P': -3, 'S': -2, 'R': -2, 'T': -1, 'W': -2,
        'V':  1, 'Y': -1, 'X': -1, 'Z': -3},

    'N':{'A': -2, 'C': -3, 'B':  3, 'E':  0,
        'D':  1, 'G':  0, 'F': -3, 'I': -3, 'H':  1,
        'K':  0, 'M': -2, 'L': -3, 'N':  6, 'Q':  0,
        'P': -2, 'S':  1, 'R':  0, 'T':  0, 'W': -4,
        'V': -3, 'Y': -2, 'X': -1, 'Z':  0},

    'Q':{'A': -1, 'C': -3, 'B':  0, 'E':  2,
        'D':  0, 'G': -2, 'F': -3, 'I': -3, 'H':  0,
        'K':  1, 'M':  0, 'L': -2, 'N':  0, 'Q':  5,
        'P': -1, 'S':  0, 'R':  1, 'T': -1, 'W': -2,
        'V': -2, 'Y': -1, 'X': -1, 'Z':  3},

    'P':{'A': -1, 'C': -3, 'B': -2, 'E': -1,
        'D': -1, 'G': -2, 'F': -4, 'I': -3, 'H': -2,
        'K': -1, 'M': -2, 'L': -3, 'N': -2, 'Q': -1,
        'P':  7, 'S': -1, 'R': -2, 'T': -1, 'W': -4,
        'V': -2, 'Y': -3, 'X': -2, 'Z': -1},

    'S':{'A':  1, 'C': -1, 'B':  0, 'E':  0,
        'D':  0, 'G':  0, 'F': -2, 'I': -2, 'H': -1,
        'K':  0, 'M': -1, 'L': -2, 'N':  1, 'Q':  0,
        'P': -1, 'S':  4, 'R': -1, 'T':  1, 'W': -3,
        'V': -2, 'Y': -2, 'X':  0, 'Z':  0},

    'R':{'A': -1, 'C': -3, 'B': -1, 'E':  0,
        'D': -2, 'G': -2, 'F': -3, 'I': -3, 'H':  0,
        'K':  2, 'M': -1, 'L': -2, 'N':  0, 'Q':  1,
        'P': -2, 'S': -1, 'R':  5, 'T': -1, 'W': -3,
        'V': -3, 'Y': -2, 'X': -1, 'Z':  0},

    'T':{'A':  0, 'C': -1, 'B': -1, 'E': -1,
        'D': -1, 'G': -2, 'F': -2, 'I': -1, 'H': -2,
        'K': -1, 'M': -1, 'L': -1, 'N':  0, 'Q': -1,
        'P': -1, 'S':  1, 'R': -1, 'T':  5, 'W': -2,
        'V':  0, 'Y': -2, 'X':  0, 'Z': -1},

    'W':{'A': -3, 'C': -2, 'B': -4, 'E': -3,
        'D': -4, 'G': -2, 'F':  1, 'I': -3, 'H': -2,
        'K': -3, 'M': -1, 'L': -2, 'N': -4, 'Q': -2,
        'P': -4, 'S': -3, 'R': -3, 'T': -2, 'W': 11,
        'V': -3, 'Y':  2, 'X': -2, 'Z': -3},

    'V':{'A':  0, 'C': -1, 'B': -3, 'E': -2,
        'D': -3, 'G': -3, 'F': -1, 'I':  3, 'H': -3,
        'K': -2, 'M':  1, 'L':  1, 'N': -3, 'Q': -2,
        'P': -2, 'S': -2, 'R': -3, 'T':  0, 'W': -3,
        'V':  4, 'Y': -1, 'X': -1, 'Z': -2},

    'Y':{'A': -2, 'C': -2, 'B': -3, 'E': -2,
        'D': -3, 'G': -3, 'F':  3, 'I': -1, 'H':  2,
        'K': -2, 'M': -1, 'L': -1, 'N': -2, 'Q': -1,
        'P': -3, 'S': -2, 'R': -2, 'T': -2, 'W':  2,
        'V': -1, 'Y':  7, 'X': -1, 'Z': -2},

    'X':{'A':  0, 'C': -2, 'B': -1, 'E': -1,
        'D': -1, 'G': -1, 'F': -1, 'I': -1, 'H': -1,
        'K': -1, 'M': -1, 'L': -1, 'N': -1, 'Q': -1,
        'P': -2, 'S':  0, 'R': -1, 'T':  0, 'W': -2,
        'V': -1, 'Y': -1, 'X': -1, 'Z': -1},

    'Z':{'A': -1, 'C': -3, 'B':  1, 'E':  4,
        'D':  1, 'G': -2, 'F': -3, 'I': -3, 'H':  0,
        'K':  1, 'M': -1, 'L': -3, 'N':  0, 'Q':  3,
        'P': -1, 'S':  0, 'R':  0, 'T': -1, 'W': -3,
        'V': -2, 'Y': -2, 'X': -1, 'Z': 4}
};


var seq2 = "ISALIGNED";
var seq1 = "THISLINE";
var gap_opening_penalty=0;
var gap_extension_penalty=-4;
var matchMatrix = BLOSUM62;

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
        return gap_opening_penalty+gap_extension_penalty;
}

function match(i,j)
{   
    return matchMatrix[seq1.charAt(i-1)][seq2.charAt(j-1)];
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





