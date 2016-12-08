%% generate the helical data

studytra_num=100;  % the number of training example
predict_test_number=100;    % the number of test example

i=(1:1:studytra_num)';   % the equation to generate the points of helical
alpha1=pi*(i-1)/25;
beta=0.4*((105-i)/104);
x0=0.5+beta.*sin(alpha1);
y0=0.5+beta.*cos(alpha1);
z0=zeros(studytra_num,1);
x1=0.5-beta.*sin(alpha1);
y1=0.5-beta.*cos(alpha1);
z1=ones(studytra_num,1);

%% It is provinced that the result of training is related to the order of +/- 
% we need to mix them.
k=rand(1,2*studytra_num);
[m,n]=sort(k);

studytra=[x0 y0 z0;x1,y1,z1]; % the point of one helical line data, a matrix of 200*3
trian_label1=studytra(n(1:2*studytra_num),end)';   %the label for the training data a vector of 1*200
studytra_data1=studytra(n(1:2*studytra_num),1:end-1)'; % the input of training data - matrix of 2*200

% change 1-D result to 2D, studytra_labe2 is a matrix of 200*2
for i=1:2*studytra_num
    switch trian_label1(i)
        case 0
            studytra_label2(i,:)=[1 0];
        case 1
            studytra_label2(i,:)=[0 1];
    end
end

[studytra_data,studytra_datas]=mapminmax(studytra_data1);%studytra_data - matrix of 2*200
studytra_label=studytra_label2'; %studytra_label- matrix of 2*200
         
plot(x0,y0,'r+');
hold on;
plot(x1,y1,'go');


%% generate the test data of helical line

i=(1.5:1:predict_test_number+0.5)';    %51 examples for each category
%the equation to generate the helical line's point
alpha2=pi*(i-1)/25;
beta2=0.4*((105-i)/104);
m0=0.5+beta2.*sin(alpha2);
n0=0.5+beta2.*cos(alpha2);
s0=zeros(predict_test_number,1);
m1=0.5-beta2.*sin(alpha2);
n1=0.5-beta2.*cos(alpha2);
s1=ones(predict_test_number,1);

predict_test=[m0 n0 s0;m1,n1,s1]; % the data point for one helical line with label - matrix of 3*102
predict_test_label1=predict_test(:,end)';   %%the label for the training data a vector of 1*102
predict_test_data1=predict_test(:,1:end-1)'; %% the input of training data - matrix of 2*102

%change 1-D result to 2D, studytra_labe2 is a matrix of 200*2
for i=1:2*predict_test_number
    switch predict_test_label1(i)
        case 0
            predict_test_label2(i,:)=[1 0];
        case 1
            predict_test_label2(i,:)=[0 1];
    end
end

predict_test_label=predict_test_label2'; %predict_test_label -  matrix of 2*102
         
%%  draw the helical lines for test data
plot(m0,n0,'c+');
hold on;
plot(m1,n1,'yo');
legend('training data - helical line1','training data - helical line2','test data - helical line1','test data - helical line2');

predict_test_data=mapminmax('apply',predict_test_data1,studytra_datas);

%% the training data and the test data are as follows
studytra_data  ;    %matrix  of 2*200
studytra_label  ;   %matrix  of 2*200
predict_test_data   ;    %matrix  of 2*200
predict_test_label   ;   %matrix  of 2*200

% %%  ues the NN-toolbox in matlab to training - method 1
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


%% ues the NN-toolbox in matlab to training - method 2
net=feedforwardnet(8);
%pay attention that each col is a property of example, the number of col is the number of example, and the name cannot have "train"/"test" to avoid the error of matlab 

net=train(net,studytra_data,studytra_label);
%view(net);
predict_label=sim(net,predict_test_data);


% %% use the model after training to predeict the data
% for i=1:2*predict_test_number
%     for j=1:midnum
%         I(j)=predict_test_data(:,i)'*w1(j,:)'+b1(j);
%         Iout(j)=1/(1+exp(-I(j)));            %Iout - vector of 1*3
%     end
%     predict_test(:,i)=w2'*Iout'+b2;          %predict_test - matrix of 2*102
% end


%% analyst of the predicion's result
for i=1:2*predict_test_number
    output_pred(i)=find(predict_label(:,i)==max(predict_label(:,i)));    %out_pred - matrix of 1*102
end

error=output_pred-predict_test_label1-1;    % error


%% calculate the sum of the wrong prediction of each category
k=zeros(1,2);          %k=[0 0]
for i=1:2*predict_test_number
    if error(i)~=0    % we couldn't use "if error(i)！=0" in matlab
        [b c]=max(predict_test_label(:,i));
        switch c
            case 1
                k(1)=k(1)+1;
            case 2
                k(2)=k(2)+1;
        end
    end
end


%% calculate the sum of the number of each category
kk=zeros(1,2);      %k=[0 0]
for i=1:2*predict_test_number
    [b c]=max(predict_test_label(:,i));
    switch c
        case 1
            kk(1)=kk(1)+1;
        case 2
            kk(2)=kk(2)+1;
    end
end


%% calculate the accuracy of each category
accuracy=(kk-k)./kk