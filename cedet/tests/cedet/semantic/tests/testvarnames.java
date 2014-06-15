/** testvarnames.java --- Test Class/Variable name overrides
 *
 * Copyright (C) 2014 Eric M. Ludlam
 *
 * Author: Eric M. Ludlam <eric@siege-engine.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/.
 */

package tests;

/**
 * 
 *
 * Created: 02/17/14
 *
 * @author Eric M. Ludlam
 * @version 
 * @since 
 */
public class testvarnames {

    public class varorclass { 
	public static long misclongvalue;
    };

    public static varorclass varoftypevarorclass = NULL;

    public static long varorclass = 1;

    public static long assignintovar = 1;

    public static varorclass classassign = NULL;

    static public void main(String [] args) {

	varorclass = assign// -1-
	    // #1# ( "assignintovar" )
	    ;

	varoftypevarorclass = clas// -2-
	    // #2# ( "classassign" )

	varoftypevarorclass.misc//-3-
	    // #3# ( "misclongvalue" )
    }

} // testvarnames
