## 跨平台问题

从ligdx调用facebook。由于ApplicationListener是跨平台的，而facebook提供的sdk是跟android绑定的，所以不能让ApplicationListener直接依赖于facebook sdk，否则就会形成跨平台的java依赖于android，这显然是反人类的。

我是这么做的，把游戏中需要的功能做成一个接口，然后在android那边去实现。

    public interface ConnectWithFacebook {
	    public void login(SessionChangeListener listener);
	    public void logout(SessionChangeListener listener);
        public boolean isConnected();
        public void getMeInformation(GetMeInformationCB callback);
        public void getFriendsInformation(GetFriendsInformationCB callback);
        public void askForLife();
        public void postLevelScore(int level, long score);
        public void postLevelScores(int[] level, long[] score);
        public void checkIncomingNotification(CheckIncomingNotificationCB callback);
    }

我们游戏中使用到的暂时只有登陆登出，获取自己的信息，获取好友信息（姓名，头像等），分数排名，向好友发送邀请等。

注意到其中的参数又是一个回调的类，比如`getMeInformation`的参数是一个`GetMeInformationCB`，这是一个接口：

    public interface GetMeInformationCB {
	    public void onComplete(FacebookUser me);
	    public void onDownloadPicture();
	    public void onFail();
    }

虽然回调有点多，但是这样子写起来不会乱。比如我知道在onDownloadPicture时facebook那边是在执行下载图片操作，那么libgdx这边就播放loading的转圈圈的画面。最后的代码结构大致是这样子的：

game下：

    public interface ConnectWithFacebook {
    }
    public class Game implements ApplicationListener {
        ConnectWithFacebook facebook;
    }

game-android下：

    public class FacebookAndriod implements ConnectWithFacebook {
    }
    public class GameAndroid extends AndroidApplication {
        public void onCreate(Bundle saved) {
            ConnectWithFacebook facebook = new FacebookAndroid();
            Game game = new Game(facebook);
            AndroidApplicationConfiguration cfg = new AndroidApplicationConfiguration();
            initialize(game, cfg);
        }
    }

## 跨线程问题

我们用接口+回调处理好了从libgdx中调用facebook sdk的功能，然后又要返回到libgdx这边更新ui。但是中间可能还涉及到一个跨线程问题。比如到facebook(Android)那边以后，要下载好友头像，会开线程做异步。

从下载图片的线程直接回到libgdx是不可以执行ui操作的，好像是会报一个没有opengl上下文的错误。这种情况可用以libgdx的函数`postRunnable`，这个是AndroidApplication类的方法。它会将一个runnable推迟到下一次render的时候执行。其实libgdx的render也不是使用的opengl线程，这个偏离主题就不说了。

类似地，如果是从libgdx过来，到FacebookAndroid的方法中，直接执行facebook sdk的`Request.executeAsync`，也会出现错误，你需要调用Activity的`runOnUiThread`函数。AndroidApplication就是你需要的Activity。

## 从创建项目到login

follow facebook官方的那个tutorial就可以了。这篇文章中讲需要注意的地方，基本部分去看官方文档就好了。

## 发送和接收request

这一块不要照着官方的Game Tutorials做，那个教程很坑爹，它只教你处理从facebook应用发送intent启动你的游戏的情况。显然，你应该绕过facebook应用，自己检查和处理请求的。

应该好好看一下Graph Api。具体地说：

{user-id}/apprequest。
