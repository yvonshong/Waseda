var seq2 = "COELACANTH";
var seq1 = "PELICAN";
//initial
var matrix= new Array(seq1.length);
for(var p=0;p<seq1.length;p++)
{
    matrix[p]=new Array(seq2.length);
}
// find the value
for(var i=0;i<seq1.length;i++)
{
    for(var j=0;j<seq2.length;j++)
    {
        matrix[i][j]=maxValue(i,j)+Match(i,j);
    }
}
//output
for(var i0=0;i0<seq1.length;i0++)
{console.log(matrix[i0]);}
//find the maxValue near this grid
function maxValue(m,n)
{
    var max=0;
    if(m==0&&n==0)
        return max=0;
    if(m==0&&n>0)
        return max=matrix[0][n-1];
    if(m>0&&n==0)
        return max=matrix[m-1][0];
    if(m*n>0)
        return max=Math.max(matrix[m-1][n-1],matrix[m][n-1],matrix[m-1][n]);
}
//find the value of match function
function Match(m,n)
{
    if(seq1.charAt(m)==seq2.charAt(n))
        return 1;
    else
        return -1;
}
