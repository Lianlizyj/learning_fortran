% function [der_x,der_y]=derive(r,z,psi)
% purpose: calculate the partial derive of psi
% input:
% output
% example:

% Author: Yanjie Zhang, based on carre module in SOLPS-ITER
% Email:lianlizyj@mail.dlut.edu.cn

%% test: prepare: r0,z0(vector)and psi(:)

%%

tmp=psi(:,1)';

fp=((tmp(3:end)-tmp(2:end-1)).*(r0(2:end-1)-r0(1:end-2))./(r0(3:end)-r0(2:end-1))...
    +(tmp(2:end-1)-tmp(1:end-2)).*(r0(3:end)-r0(2:end-1))./...
    (r0(3:end)-r0(2:end-1)))./(r0(3:end)-r0(1:end-2));
r2=r0(2)-r0(1);
r3=r0(3)-r0(1);
f2=tmp(2)-tmp(1);
f3=tmp(3)-tmp(1);
fp1=(f2*r3^2-f3*r2^2)/(r2*r3*(r3-r2));

r2=r0(end-1)-r0(end);
r3=r0(end-2)-r0(end);
f2=tmp(end-1)-tmp(end);
f3=tmp(end-2)-tmp(end);
fpn=(f2*r3^2-f3*r2^2)/(r2*r3*(r3-r2));

fp_tot=[fp1,fp,fpn];

df_tmp=diff(tmp,1);
grad_tmp=gradient(tmp,r0);

plot(fp_tot,'r-')
hold on
plot(grad_tmp,'g-.')






% end