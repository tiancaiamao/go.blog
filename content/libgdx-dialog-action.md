libgdx中弹窗出来和收回的动画效果一般是用自带的action系统做的，要把这个东西做到不犯错还真不容易。把我自己容易踩到的坑记录一下，避免以后继续踩。

1. 加了action却没有弹窗动作

这种情况很可能是add进group之前，把action给clear掉了。很容易不小心就是先加了一遍action，结果在加到group中之前又给clear掉了。

弹出动作处理正确顺序是:

* 清除掉dialog之前的action(保险)
* 执行dialog的回调，回调函数中添加action
* 将dialog添加到group中

2. 重复上次弹窗收回未执行完的动作

这种情况发生的大多是收回没处理好。比较保守的做法是在弹出之前先clear一下action，防止有上次还没运行完的action继续执行。

还要特别注意，谁分配谁释放的原则--很可能action不是加在dialog中的，而是加到了dialog里的东西中的。那么一定要在dialog自己的收回的回调函数中消除。

3. 没有从group中remove就清除掉action导致崩溃

action的工作原理是在actor的act函数中调用，并且如果action完成时，会调用remov清除掉这个action。如果手动remove掉了action，而actor还在act，那么当action执行完时会再remove，remove两次导致崩溃。

4. 正确的收回处理

收回的动作比弹出还要容易出错一些。主要是要在action执行完之后才可以其它的代码。正确的处理流程应该是：

* 将hide之后要执行的运行封装到一个runnable里面
* 执行dialog的回调函数，添加action
* Actions.sequence执行现在的action，然后是run中remove掉dialog并执行那个runnable

主要是action跟runnable交织在一起了。