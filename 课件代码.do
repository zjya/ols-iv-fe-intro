* ols
cd I:\STATA15ANZHUANG\ado\plus\junior
use grilic.dta, clear
twoway scatter lnw s || lfit lnw s
reg lnw s

*蒙特卡洛模拟
clear  //删除内存中已有的数据
set obs 30  //确定随机抽样的样本容量n=30
set seed 10101  //指定随机抽样的“种子”为10101
gen x = rnormal(3,4)  //得到服从N(3,4)分布的随机样本
gen e = rnormal(0,9) //得到服从N(0,9)的随机样本
gen y = 1+2*x+e
reg y x
twoway function PRF = 1+2*x, range(-5 15) || scatter y x || lfit y x, lpattern(dash)

*多元线性回归：估计科布道格拉斯函数
use cobb_douglas.dta, clear
reg lny lnk lnl
*得到拟合值和残差
predict lny1
predict e, residual
line lny lny1 year, lpattern(solid dash) //year是横轴，lny为实线，lny1为虚线

use grilic.dta, clear
reg lnw s expr tenure smsa rns
vce //variance covariance matrix estimated 
*子样本回归
reg lnw s expr tenure smsa if rns==1
reg lnw s expr tenure smsa if ~rns
reg lnw s expr tenure smsa rns if s >=12
reg lnw s expr tenure smsa if s >=12 & rns
quietly reg lnw s expr tenure smsa rns
test s = 0.1

*大样本OLS
use nerlove.dta, clear
reg lntc lnq lnpl lnpk lnpf
test lnq = 1
reg lntc lnq lnpl lnpk lnpf,r

*面板数据回归
*先设定面板数据
xtset panelvar timevar
xtdes //显示面板的数据结构
xtsum //显示组内、组间与整体的统计指标
xtline varname //对每位个体分别显示该变量的时间序列图
xtline varname, overlay

*家庭联产承包责任制对中国农业增长的影响
use lin_1992.dta, clear
xtset province year
xtdes                      
xtline ltvfo
xtline ltvfo, overlay
*混合回归（肯定不会做混合回归）
reg y x1 x2 x3, vce(cluster id) //id用来确定每位个体的变量，而选择项“vce(cluster id)”表示以变量id做为聚类变量来计算聚类稳健标准误。
reg ltvfo ltlan ltwlab ltpow ltfer hrs mipric1 giprice mci ngca, vce(cluster province)
reg ltvfo ltlan ltwlab ltpow ltfer hrs mipric1 giprice mci ngca //普通标准误会导致统计检验失效
*固定效应 去除不随时间而变的遗漏变量的影响，任何不随时间而变化的解释变量也将随着遗漏变量一道被消除掉，固定效应模型是一种折中的估计策略
*固定效应模型的组内估计法
xtreg y x1 x2 x3, fe r //fe表示fixed effects,r表示使用聚类稳健标准误
*LSDV法的命令
reg y x1 x2 x3 i.id, vce(cluster id) //id表示用来确定个体的变量，“i.id”则表示根据变量id产生虚拟变量。选择项“vce(cluster id)”表示使用聚类稳健标准误。
xtreg ltvfo ltlan ltwlab ltpow ltfer hrs mipric1 giprice mci ngca, fe r
estimates store FE_robust
*LSDV法，由于加入了虚拟变量，所以可以得到对个体异质性ui的估计
reg ltvfo ltlan ltwlab ltpow ltfer hrs mipric1 giprice mci ngca i.province, vce(cluster province) //注意回归结果是27个省
estimates store LSDV //不少个体的虚拟变量在5%水平上显著，故可以拒绝使用混合回归

*进阶命令areg
areg depvar [indepvars] [if] [in] [weight], absorb(varname) [options] //areg命令可以在回归中直接控制虚拟变量，而无需真的去生成这些虚拟变量，回归结果和在reg中直接加入虚拟变量相同

*加入时间固定效应
xtreg ltvfo ltlan ltwlab ltpow ltfer hrs mipric1 giprice mci ngca t, fe r
estimates store FE_trend
*由于以上结果的时间虚拟变量不显著，我们采用时间虚拟变量
*生成时间虚拟变量的简洁命令tab
tab year, gen(year)
xtreg ltvfo ltlan ltwlab ltpow ltfer hrs mipric1 giprice mci ngca year2-year18, fe r
estimates store FE_TW
test year2 year3 year4 year5 year6 year7 year8 year9 year10 year11 year12 year13 year14 year15 year16 year17 year18
*双向固定效应模型的其他命令
xtreg ltvfo ltlan ltwlab ltpow ltfer hrs mipric1 giprice mci ngca i.year, fe r 
*以上结果表明不能够使用混合回归，无论是截面上还是时间上都存在异质性
*随机效应模型的命令
xtreg y x1 x2 x3, re r theta //theta
*随机效应模型也可以使用极大似然估计
xtreg y x1 x2 x3, mle
xtreg ltvfo ltlan ltwlab ltpow ltfer hrs mipric1 giprice mci ngca, re r theta
estimates store RE_robust
*豪斯曼检验
xtreg y x1 x2 x3, fe //固定效应估计
estimates store FE //存储结果
xtreg y x1 x2 x3, re //随机效应估计
estimates store RE //存储结果
hausman FE RE, constant sigmamore //constant表示在比较系数估计值时包括常数项；sigmamore表示统一使用更有效率的那个估计量(即随机效应估计量)的方差估计。
*由于传统的豪斯曼检验假设同方差、无自相关的随机误差项，故在进行固定效应与随机效应的估计时，均不使用异方差或聚类稳健的标准误。
xtreg ltvfo ltlan ltwlab ltpow ltfer hrs mipric1 giprice mci ngca, fe
estimates store FE
xtreg ltvfo ltlan ltwlab ltpow ltfer hrs mipric1 giprice mci ngca, re
estimates store RE 
hausman FE RE, constant sigmamore
*但是以上的豪斯曼检验不能适用于异方差情形
quietly xtreg ltvfo ltlan ltwlab ltpow ltfer hrs mipric1 giprice mci ngca, r 
xtoverid //拒绝原假设，应采用固定效应模型
*列示结果
esttab FE_robust FE_trend FE RE, b se mtitle
reg2docx FE_robust FE_trend FE RE using I:/mytable1.docx, replace se  mtitles("FE_robust" "FE_trend" "FE" "RE") star(* 0.1 ** 0.05 *** 0.01)

*进阶命令reghdfe
ssc install reghdfe
*语法
reghdfe depvar [indepvars] [if] [in] [weight], absorb(absvars) [options] //absorb(absvars):引入固定效应
*可以包含多维固定效应，absorb(Var1,Var2,Var3,…)，absorb(Var1,Var2,FE3=Var3),变量FE3将保存var3的固定效应估计结果
*优点：可以对多维固定效应进行分析(如，公司-年度-高管，省份-城市-行业-年度)；计算速度快


*工具变量法的两阶段最小二乘法
2SLS的命令格式
ivregress 2sls y x1 x2 (x3 = z1 z2), robust first
*当随机误差项是同方差、无自相关的时候，2SLS是最有效率的工具变量法。然而，在异方差的情况下，则存在更有效率的工具变量法，即广义矩估计(GMM).
检验弱工具变量
estat firststage
*对弱工具变量不敏感的有限信息最大似然估计法
ivregress liml y x1 x2 (x3 = z1 z2)
*过度识别检验
estate overid 
*检验变量的内生性：豪斯曼检验
reg y x1 x2 
estimates store OLS
ivregress 2sls y x1 (x2 = z1 z2)
estimates store IV
hausman IV OLS, constant sigmamore
*同样不适合异方差情况下的检验，异方差稳健的DWH检验：
estat endogenous

use grilic.dta, clear
reg lnw s expr tenure rns smsa, r 
*可能存在遗漏变量，引入智商（iq）作为能力的代理变量
reg lnw s iq expr tenure rns smsa, r 
ivregress 2sls lnw s expr tenure rns smsa (iq = med kww), r first
estat overid //两个工具变量，进行过度识别检验
*弱工具变量检验
quietly ivregress 2sls lnw s expr tenure rns smsa (iq = med kww) //注意这里没有使用稳健标准误
estat firststage //F统计量大于10，就可以通过弱工具变量检验

*最后检验变量的内生性：豪斯曼检验
quietly reg lnw iq s expr tenure rns smsa
est sto OLS
quietly ivregress 2sls lnw s expr tenure rns smsa (iq = med kww)
est sto IV
hausman IV OLS, constant sigmamore
*DWH检验
estat endogenous

*进阶版
cd I:\STATA15ANZHUANG\ado\plus\senior
*当随机误差项同方差、无自相关时，2SLS是最有效率的。但如果随机误差项存在异方差或者自相关，则存在更有效的方法，即“广义矩估计”（GMM）
*GMM的命令
ivregress gmm y x1 (x2 = z1 z2) //两步GMM
ivregress gmm y x1 (x2 = z1 z2), igmm //迭代GMM
estat overid //过度识别检验
*使用异方差自相关稳健的标准误的GMM命令
ivregress gmm y x1 (x2 = z1 z2 ),vce(hac nwest[#])

use grilic.dta, clear
sum
pwcorr iq s, sig  //sig表示显示显著性水平
reg lw s expr tenure rns smsa, r
*iq作为能力的代理变量
reg lw s iq expr tenure rns smsa, r
ivregress 2sls lw s expr tenure rns smsa (iq = med kww mrt age), r
estat overid
ivreg2 lw s expr tenure rns smsa (iq = med kww mrt age), r orthog (mrt age) //该命令默认估计量为2SLS，r表示异方差稳健标准误，orthog (mrt age)表示检验(mrt age)是否满足外生性
ivreg2 lw s expr tenure rns smsa (iq = med kww mrt age),gmm2s robust orthog (mrt age)
*考虑仅使用变量（med kww)作为iq的工具变量，再次进行2SLS回归，同时显示第一阶段的回归结果
ivregress 2sls lw s expr tenure rns smsa (iq = med kww), r first
estat overid //接受了原假设
*弱工具变量检验
estat firststage, all forcenonrobust //该命令将显示与弱工具变量有关的第一个阶段回归统计量及临界值。all表示显示每个内生变量的统计量，而非仅仅是所有内生变量综合的统计量。forcenonrobust表示，即使在进行工具变量法时用了稳健标准误(robust),也仍然允许计算estat firststage中的统计量（这些统计量基于同方差的假设）。注意，这里是强行计算，其实我也质疑这种方法的有效性。
*用ivreg2来进行弱工具变量检验
ivreg2 lw s expr tenure rns smsa (iq = med kww), r redundant (kww)
*用ivreg2来进行解释变量的内生性检验
ivreg2 lw s expr tenure rns smsa (iq = med kww), r endog(iq)  //endog(iq)表示检验变量iq是否为内生变量
*如果存在异方差，则GMM比2SLS更有效率
ivreg2 lw s expr tenure rns smsa (iq = med kww), gmm2s robust
