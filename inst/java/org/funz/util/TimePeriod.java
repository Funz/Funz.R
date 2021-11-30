package org.funz.util;

import java.util.Calendar;

public class TimePeriod {
	final private long start, end;

	final private String tstart, tend;

	public TimePeriod(String t1, String t2) {
		start = hourToLong(t1);
		end = hourToLong(t2);
		tstart = t1;
		tend = t2;
	}

	public String getEndString() {
		return tend;
	}

	public String getStartString() {
		return tstart;
	}

	private long hourToLong(String hour) {
		return Long.parseLong(hour.substring(0, 2)) * 3600000L + Long.parseLong(hour.substring(3)) * 60000L;
	}

	public boolean isInside(long t) {
		if (end > start) {
			return t > start && t < end;
		} else {
			return (t > start && t < (24L * 3600000L)) || t < end;
		}
	}

	public boolean isNow() {
		Calendar c = Calendar.getInstance();
		return isInside(c.get(Calendar.HOUR_OF_DAY) * 3600000L + c.get(Calendar.MINUTE) * 60000L);
	}
}
