/*
 * RecordStoreEnumeration support library.
 * */

import javax.microedition.rms.*;

public class Lib_rsenum
{
	/*
	* function EnumerateRecords(rs: RecordStore): integer;
	*
	* Enumerates all records in a record store and returns the index of a record
	* store enumeration. Returns -1 on an error.
	*
	* When record store enumeration is no longer needed, CloseRSEnumeration should
	* be called.
	* */
	public static int enumeraterecords(RecordStore rs)
	{
		try
		{
			RecordEnumeration rsenum = rs.enumerateRecords(null, null, false);
			
			int guardCnt = 0;
			while (records[nextRecord] != null)
			{
				nextRecord ++;
				if (nextRecord == 32) nextRecord = 0;
				guardCnt = 0;

				if (guardCnt > 32)
					return -1;
			}

			records[nextRecord] = rsenum;
			int idx = nextRecord;
			
			nextRecord ++;
			if (nextRecord == 32) nextRecord = 0;

			return idx;
		}
		catch (Exception e)
		{
			return -1;
		}
	}

	/*
	 * function NextRecord(idx: integer): string
	 *
	 * Returns the value of the next record store entry in the record store
	 * enumeration. Returns an empty string on error or if there are no more 
	 * records. 'idx' is the index of the record store enumeration.
	 * */
	public static String nextrecord(int idx)
	{
		try
		{
			RecordEnumeration rsenum = records[idx];
			return new String(rsenum.nextRecord());
		}
		catch (Exception e)
		{
			return "";
		}
	}

	/*
	 * procedure CloseRSEnumeration(idx: integer)
	 *
	 * Closes a record store enumeration.
	 * */
	public static void closersenumeration(int idx)
	{
		try
		{
			records[idx].destroy();
			records[idx] = null;
		}
		catch (Exception e)
		{}
	}

	/*
	 * procedure UpdateRSEnumeration(idx: integer)
	 *
	 * Updates a record store enumeration so that it reflects the current
	 * record set. 
	 * */
	public static void updatersenumeration(int idx)
	{
		try
		{
			records[idx].rebuild();
		}
		catch (Exception e)
		{}
	}

	static RecordEnumeration[] records = new RecordEnumeration[32];
	static int nextRecord = 0;

	
}
