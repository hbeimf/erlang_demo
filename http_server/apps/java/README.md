
https://mvnrepository.com/artifact/clojang/erlang-jinterface/1.7.1

javac -cp /web/yaf/socketio/http_server/apps/java/java_src/erlang-jinterface-1.7.1.jar JInterfaceExample.java





maven使用本地jar包
http://www.cnblogs.com/rongfengliang/p/5959456.html
http://www.cnblogs.com/hujunzheng/p/5659410.html


安装本地jar本：

mvn install:install-file -DgroupId=com.jinterface -DartifactId=jinterface -Dversion=18.3 -Dfile=/web/yaf/socketio/http_server/apps/java/java_src/OtpErlang.jar -Dpackaging=jar -DgeneratePom=true


<dependency>
        <groupId>com.jinterface</groupId>
        <artifactId>jinterface</artifactId>
        <version>18.3</version>
    </dependency>





java -jar java-node-0.0.1-jar-with-dependencies.jar com.node.java.JavaNode 






























install erlang

otpErlang.jar



http://www.sjsjw.com/kf_code/article/032953ABA020336.asp

http://www.erlang.org/doc/installation_guide/INSTALL.html#id62915


http://amornio.iteye.com/blog/1551502


Java代码  收藏代码

    # tar -zxvf otp_src_R15B01.tar.gz
    # cd otp_src_R15B01



安装依赖：
Java代码  收藏代码

    # yum install build-essential m4 openssl openssl-devel unixODBC unixODBC-devel make gcc gcc-c++ kernel-devel ncurses-devel



配置configure
Java代码  收藏代码

    # ./configure --prefix=/usr/local/erlang_18.3 --enable-hipe --enable-threads --enable-smp-support --enable-kernel-poll --with-opengl --enable-debug --enable-unicode --with-javac


    make

    make install



完成之后，设置环境变量

Java代码  收藏代码

    vim /etc/profile

    ERL_HOME=/usr/local/erlang
    PATH=$ERL_HOME/bin:$PATH
    export ERL_HOME PATH



完成后保存

Java代码  收藏代码

    source /etc/profile































