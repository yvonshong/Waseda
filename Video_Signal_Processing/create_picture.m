P=imread('pic0.jpg');         
imshow(P,[])      

P1=imnoise(P,'gaussian',0.02);     
imshow(P1,[])   
imwrite(P1,'D:\document\Waseda\Video_Signal_Processing\img\1.1.jpg');

P2=imnoise(P,'salt & pepper',0.02); 
imshow(P2,[]) 
imwrite(P2,'D:\document\Waseda\Video_Signal_Processing\img\1.2.jpg');

G=rgb2gray(P);
imshow(G,[])
imwrite(G,'D:\document\Waseda\Video_Signal_Processing\img\2.0.jpg');

G1=imnoise(G,'gaussian',0.02);
imshow(G1,[])    
imwrite(G1,'D:\document\Waseda\Video_Signal_Processing\img\2.1.jpg');

G2=imnoise(G,'salt & pepper',0.02);
imshow(G2,[]) 
imwrite(G2,'D:\document\Waseda\Video_Signal_Processing\img\2.2.jpg');