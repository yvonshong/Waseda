%% generate the helical data, each category has 100 examples
train_num=100;
train_circle_number=5000;
test_number=100;
i=(1:1:train_num)';
% the equation to generate the points of helical
alpha1=pi*(i-1)/25;
beta=0.4*((105-i)/104);
x0=0.5+beta.*sin(alpha1);
y0=0.5+beta.*cos(alpha1);
z0=zeros(train_num,1);
x1=0.5-beta.*sin(alpha1);
y1=0.5-beta.*cos(alpha1);
z1=ones(train_num,1);
%% It is provinced that the result of training is related to the order of +/- 
% we need to mix them.
k=rand(1,2*train_num);
[m,n]=sort(k);

train=[x0 y0 z0;x1,y1,z1];                     % the point of one helical line data, a matrix of 200*3
trian_label1=train(n(1:2*train_num),end)';     % the label for the training data a vector of 1*200
train_data1 =train(n(1:2*train_num),1:end-1)'; % the input of training data - matrix of 2*200

% change 1-D result to 2D, studytra_labe2 is a matrix of 200*2
for i=1:2*train_num
    switch trian_label1(i)
        case 0
            train_label2(i,:)=[1 0];
        case 1
            train_label2(i,:)=[0 1];
    end
end

train_label=train_label2'; %train_label - matrix of 2*200
         
plot(x0,y0,'r+');
hold on;
plot(x1,y1,'go');
%legend();

%% initial the structure of BP nuerual network
%network structure - 2 inputs 3 nueron and 2 outputs
innum=2;
midnum=5;
outnum=2;

[train_data,train_datas]=mapminmax(train_data1);

%输入输出取值阈值随机初始化
%w1矩阵表示每一行为一个隐含层神经元的输入权值
w1=rands(midnum,innum);  %rands函数用来初始化神经元的权值和阈值是很合适的,w1为3*2的矩阵
b1=rands(midnum,1);      %b1为3*1的矩阵
%w2矩阵表示每一列为一个输出层神经元的输入权值
w2=rands(midnum,outnum); %w2为3*2的矩阵
b2=rands(outnum,1);      %b2为2*1的矩阵

%用来保存上一次的权值和阈值，因为后面的更新方差是递归的，要用到
w1_1=w1;w1_2=w1_1;
b1_1=b1;b1_2=b1_1;
w2_1=w2;w2_2=w2_1;
b2_1=b2;b2_2=b2_1;

%学习率的设定
alpha=0.05;

%训练10次就ok了，而不管训练后的结果如何
for train_circle=1:train_circle_number  ;
    for i=1:2*train_num; %200个训练样本
       %% 输入层的输出
        x=train_data(:,i);%取出第i个样本，x(i)为2*1的列向量
        %% 隐含层的输出
        for j=1:midnum;
            I(j)=train_data(:,i)'*w1(j,:)'+b1(j);  %I(j)为1*1的实数
            Iout(j)=1/(1+exp(-I(j)));   %Iout(j)也为1*1的实数
        end     %Iout为1*3的行向量   
        %% 输出层的输出
         yn=(Iout*w2)'+b2;   %yn为2*1的列向量，因此此时的传函为线性的，所以可以一步到位，不必上面
        
        %% 计算误差
        e=train_label(:,i)-yn; %e为2*1的列向量，保存的是误差值
        
        %计算权值变换率
        dw2=e*Iout; %dw2为2*3的矩阵，每一行表示输出接点的输入权值变化率
        db2=e'; %e为1*2的行向量
        
        for j=1:midnum
            S=1/(1+exp(-I(j)));
            FI(j)=S*(1-S);  %FI(j)为一实数，FI为1*3的行向量
        end
        
        for k=1:1:innum
            for j=1:midnum
                dw1(k,j)=FI(j)*x(k)*(e(1)*w2(j,1)+e(2)*w2(j,2));    %dw1为2*3的矩阵
                db1(j)=FI(j)*(e(1)*w2(j,1)+e(2)*w2(j,2));   %db1为1*3的矩阵
            end
        end
        
        %% 权值更新方程
        w1=w1_1+alpha*dw1'; %w1仍为3*2的矩阵
        b1=b1_1+alpha*db1'; %b1仍为3*1的矩阵
        w2=w2_1+alpha*dw2'; %w2仍为3*2的矩阵
        b2=b2_1+alpha*db2'; %b2仍为2*1的矩阵
        
        %% 保存上一次的权值和阈值
        w1_2=w1_1;w1_1=w1;
        b1_2=b1_1;b1_1=b1;
        w2_2=w2_1;w2_1=w2;
        b2_2=b2_1;b2_1=b2;
    end
end


%% 产生双螺旋测试数据
%% 产生双螺旋数据,每类100个样本点，共200个样本
i=(1.5:1:test_number+0.5)';    %每类51个样本

%双螺旋数据点的产生方程
alpha2=pi*(i-1)/25;
beta2=0.4*((105-i)/104);
m0=0.5+beta2.*sin(alpha2);
n0=0.5+beta2.*cos(alpha2);
s0=zeros(test_number,1);
m1=0.5-beta2.*sin(alpha2);
n1=0.5-beta2.*cos(alpha2);
s1=ones(test_number,1);

test=[m0 n0 s0;m1,n1,s1];    %1条螺旋线数据点,3*102的矩阵
test_label1=test(:,end)';    %测试数据类别，1*102的行向量
test_data1=test(:,1:end-1)'; %测试数据属性，2*102的矩阵

%把1维的输出变成2维的输出,train_labe2为200*2的矩阵
for i=1:2*test_number
    switch test_label1(i)
        case 0
            test_label2(i,:)=[1 0];
        case 1
            test_label2(i,:)=[0 1];
    end
end

test_label=test_label2'; %test_label为2*102的矩阵
         
%%  画出测试数据双螺旋曲线
plot(m0,n0,'c+');
hold on;
plot(m1,n1,'yo');
legend('training data - helical line1','training data - helical line2','test data - helical line1','test data - helical line2');

test_data=mapminmax('apply',test_data1,train_datas);

% %% 用训练到的模型对训练数据本身进行预测
% for i=1:102
%     for j=1:midnum
%         I(j)=train_data(:,i)'*w1(j,:)'+b1(j);
%         Iout(j)=1/(1+exp(-I(j)));%Iout为1*3的行向量
%     end
%     predict(:,i)=w2'*Iout'+b2;%predict为2*102的矩阵
% end
% 
% test_data=mapminmax('apply',train_data1,train_datas);
% test_label=train_label;
% test_label1=trian_label1;

%% 用训练到的模型预测数据
for i=1:2*test_number
    for j=1:midnum
        I(j)=test_data(:,i)'*w1(j,:)'+b1(j);
        Iout(j)=1/(1+exp(-I(j)));%Iout为1*3的行向量
    end
    predict(:,i)=w2'*Iout'+b2;%predict为2*102的矩阵
end

%% 预测结果分析
for i=1:2*test_number
    output_pred(i)=find(predict(:,i)==max(predict(:,i)));    %out_pred为1*102的矩阵
end

error=output_pred-test_label1-1;    %


%% 计算出每一类预测错误的个数总和
k=zeros(1,2); %k=[0 0]
for i=1:2*test_number
    if error(i)~=0    %matlab中不能用if error(i)！=0 
        [b c]=max(test_label(:,i));
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
for i=1:2*test_number
    [b c]=max(test_label(:,i));
    switch c
        case 1
            kk(1)=kk(1)+1;
        case 2
            kk(2)=kk(2)+1;
    end
end


%% 计算每一类的正确率
accuracy=(kk-k)./kk