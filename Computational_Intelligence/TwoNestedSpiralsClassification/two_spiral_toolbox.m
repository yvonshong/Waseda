%% 产生双螺旋数据

studytra_num=100;  %训练样本数目
predict_test_number=100;    %测试样本数目

i=(1:1:studytra_num)';
%双螺旋数据点的产生方程
alpha1=pi*(i-1)/25;
beta=0.4*((105-i)/104);
x0=0.5+beta.*sin(alpha1);
y0=0.5+beta.*cos(alpha1);
z0=zeros(studytra_num,1);
x1=0.5-beta.*sin(alpha1);
y1=0.5-beta.*cos(alpha1);
z1=ones(studytra_num,1);

%% 事实证明BP神经网络在训练数据时与输入数据正负样本的顺序是有关系的
% 如果一开始的一半数据都是正(负)样本，后面的全是负(正)样本，则训练出来的
%效果不好，所以这里需要随机打乱
k=rand(1,2*studytra_num);
[m,n]=sort(k);

studytra=[x0 y0 z0;x1,y1,z1]; %1条螺旋线数据点,200*3的矩阵
trian_label1=studytra(n(1:2*studytra_num),end)';   %训练数据类别，1*200的行向量
studytra_data1=studytra(n(1:2*studytra_num),1:end-1)'; %训练数据属性，2*200的矩阵

%把1维的输出变成2维的输出,studytra_labe2为200*2的矩阵
for i=1:2*studytra_num
    switch trian_label1(i)
        case 0
            studytra_label2(i,:)=[1 0];
        case 1
            studytra_label2(i,:)=[0 1];
    end
end

[studytra_data,studytra_datas]=mapminmax(studytra_data1);%studytra_data为2*200的矩阵
studytra_label=studytra_label2'; %studytra_label为2*200的矩阵
         
plot(x0,y0,'r+');
hold on;
plot(x1,y1,'go');


%% 产生双螺旋测试数据

i=(1.5:1:predict_test_number+0.5)';    %每类51个样本
%双螺旋数据点的产生方程
alpha2=pi*(i-1)/25;
beta2=0.4*((105-i)/104);
m0=0.5+beta2.*sin(alpha2);
n0=0.5+beta2.*cos(alpha2);
s0=zeros(predict_test_number,1);
m1=0.5-beta2.*sin(alpha2);
n1=0.5-beta2.*cos(alpha2);
s1=ones(predict_test_number,1);

predict_test=[m0 n0 s0;m1,n1,s1]; %1条螺旋线数据点,3*102的矩阵
predict_test_label1=predict_test(:,end)';   %测试数据类别，1*102的行向量
predict_test_data1=predict_test(:,1:end-1)'; %测试数据属性，2*102的矩阵

%把1维的输出变成2维的输出,studytra_labe2为200*2的矩阵
for i=1:2*predict_test_number
    switch predict_test_label1(i)
        case 0
            predict_test_label2(i,:)=[1 0];
        case 1
            predict_test_label2(i,:)=[0 1];
    end
end

predict_test_label=predict_test_label2'; %predict_test_label为2*102的矩阵
         
%%  画出测试数据双螺旋曲线
plot(m0,n0,'c+');
hold on;
plot(m1,n1,'yo');
legend('训练数据螺旋线1','训练数据螺旋线2','测试数据螺旋线1','测试数据螺旋线2');

predict_test_data=mapminmax('apply',predict_test_data1,studytra_datas);

%% 至此，训练和测试数据如下所示：
studytra_data  ;    %2*200的矩阵
studytra_label  ;   %2*200的矩阵
predict_test_data   ;    %2*200的矩阵
predict_test_label   ;   %2*200的矩阵

% %%  调用matlab的神经网络工具箱函数用来训练,方法1
% net=newff(minmax(studytra_data),[10,2],{'tansig','purelin','studytragdm'})
% inputWeights=net.IW{1,1};
% inputbias=net.b{1};
% layerWeights=net.LW{2,1};
% layerbias=net.b{2};
% 
% net.studytraParam.show=50;
% net.studytraParam.lr=0.05;
% net.studytraParam.mc=0.9;
% net.studytraParam.epochs=1000;
% net.studytraParam.goal=1e-2;
% 
% [net,tr]=studytra(net,studytra_data,studytra_label);


%% 调用matlab的神经网络工具箱函数用来训练,方法2
net=feedforwardnet(8);
%注意此时进入train函数的样本每一列为一个样本的属性，列数为样本数，且命名时最好不要含有train,test字样，否则matlab会报错
net=train(net,studytra_data,studytra_label);
%view(net);
predict_label=sim(net,predict_test_data);


% %% 用训练到的模型预测数据
% for i=1:2*predict_test_number
%     for j=1:midnum
%         I(j)=predict_test_data(:,i)'*w1(j,:)'+b1(j);
%         Iout(j)=1/(1+exp(-I(j)));%Iout为1*3的行向量
%     end
%     predict_test(:,i)=w2'*Iout'+b2;%predict_test为2*102的矩阵
% end


%% 预测结果分析
for i=1:2*predict_test_number
    output_pred(i)=find(predict_label(:,i)==max(predict_label(:,i)));    %out_pred为1*102的矩阵
end

error=output_pred-predict_test_label1-1;    %


%% 计算出每一类预测错误的个数总和
k=zeros(1,2); %k=[0 0]
for i=1:2*predict_test_number
    if error(i)~=0    %matlab中不能用if error(i)！=0 
        [b c]=max(predict_test_label(:,i));
        switch c
            case 1
                k(1)=k(1)+1;
            case 2
                k(2)=k(2)+1;
        end
    end
end


%% 求出每一类总体的个数和
kk=zeros(1,2); %k=[0 0]
for i=1:2*predict_test_number
    [b c]=max(predict_test_label(:,i));
    switch c
        case 1
            kk(1)=kk(1)+1;
        case 2
            kk(2)=kk(2)+1;
    end
end


%% 计算每一类的预测正确率
accuracy=(kk-k)./kk