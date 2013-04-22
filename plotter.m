close all; clear; 

load out0.stat;

%%x = 1:100;
x = 1:length(out0);
y = out0(:,2);

figure; plot(x,y)
