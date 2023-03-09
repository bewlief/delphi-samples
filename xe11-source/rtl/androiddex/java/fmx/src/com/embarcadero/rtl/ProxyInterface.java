package com.embarcadero.rtl;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

//import android.util.Log;

public class ProxyInterface implements InvocationHandler {
    long pointer;

    public Object CreateProxyClass(Class listenerClass, long pointer) throws ClassNotFoundException {
        this.pointer = pointer;
        //Log.e("Proxy", "Creating proxy: " + listenerClass.toString());
        return Proxy.newProxyInstance(listenerClass.getClassLoader(), new Class[] { listenerClass }, this);
    }

    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        //Log.e("Proxy", "Invoke: " + method.getName() + " --> " + method.toString());
        Object obj = dispatchToNative2(method.getName(), NativeDispatchHelper.getMethodSignature(method.getReturnType(), method.getParameterTypes()), args, pointer);
        cleanNative(pointer);
        return obj;
    }

    public native Object dispatchToNative2(String methodName, String methodSig, Object[] args, long pointer);

    public native void cleanNative(long pointer);
}
