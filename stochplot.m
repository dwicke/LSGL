close all; clear; 

load out0.stat;

%%x = 1:100;
idy = (out0(:,1)==0);
y = out0(idy,:);

x = 1:length(y);
%y = out0(:,2);

figure; 
plot(x,y(:,2))
%plot(x,y(:,3))
%plot(x,y(:,4))
%plot(x,y(:,5))
