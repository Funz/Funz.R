package org.funz;

import java.io.*;

public class Constants
{
    public final static int 
        DEFAULT_PORT = 19000;
    
    public static final String 
        APP_NAME_PROPERTY = "app.name",
        APP_HOME_PROPERTY = "app.home",
        APP_USER_PROPERTY = "app.user",
        APP_VERSION = "1.14",
        APP_BUILD_DATE = "27/11/2021 11:44";

    public static String APP_NAME = "Funz";

    public final static String INFO_FILE           = "info.line";
    public final static String INPUT_DIR           = "input";
    public final static String OUTPUT_DIR          = "output";
    public final static String PLUGINS_DIR         = "plugins";
    public final static String CALC_SUBDIR         = "calculator";
    public final static String IO_SUBDIR         = "io";
    public final static String DOE_SUBDIR         = "doe";
    public final static String TMP_SUBDIR         = "tmp";

    
    public static final File APP_INSTALL_DIR;
    public static final File APP_USER_DIR;
    public static final File USER_TMP_DIR;

    static
    {
        APP_NAME = System.getProperty( APP_NAME_PROPERTY, "Funz" );   
 
        String home = System.getProperty( APP_HOME_PROPERTY );
        
        if( home == null || home.length() == 0 ) 
            APP_INSTALL_DIR = new File( System.getProperty( "user.home" ) + File.separator + APP_NAME );
        else
            APP_INSTALL_DIR = new File( home );

        
        home = System.getProperty( APP_USER_PROPERTY );
        
        if( home == null || home.length() == 0 ) 
            APP_USER_DIR =new File( System.getProperty( "user.home" ) + File.separator + "."+APP_NAME );//PROJECT_INSTALL_DIR;
        else
            APP_USER_DIR = new File( home );

        USER_TMP_DIR =  new File( APP_USER_DIR, TMP_SUBDIR );
    }
        
    public static void main(String[] args) {
		System.out.println(APP_VERSION + " "+APP_BUILD_DATE);
	}
}
