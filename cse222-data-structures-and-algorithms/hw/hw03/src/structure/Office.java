package src.structure;

public class Office extends Building {
    private String _jobType;
    
    /**
     * Initializes the Office with given parameters
     * @param location
     * @param width
     * @param heigth
     * @param owner
     * @param jobType
     */
    public Office(int location, int width, int heigth, String owner, String jobType) {
        super(location, width, heigth, owner);
        setJobType(jobType);
    }

    /**
     * Initializes the Office with given parameters
     * @param location
     * @param width
     * @param heigth
     * @param owner
     */
    public Office(int location, int width, int heigth, String owner) {
        this(location, width, heigth, owner, null);
    }

    /**
     * Initializes the office with default values
     */
    public Office() {
        this(0, 0, 0, null, null);
    }

    /**
     * Set job type of office
     * @param jobType
     */
    public void setJobType(String jobType) {_jobType = jobType;}

    /**
     * Job type
     * @return
     */
    public String getJobType() {return _jobType;}

    @Override
    public String toString() {
        return String.format("%sJob Type: %s\n", super.toString(), _jobType);
    }

    public int hashCode() {
        return 31 *(super.hashCode() + 
            ((_jobType != null && _jobType.length() > 0) ? _jobType.charAt(0) : 0));
    }

    @Override
    public boolean equals(Object obj) {
        if (super.equals(obj)) {
            Office other = (Office) obj;
            if (_jobType == null)
                return other._jobType == null;
            else
                return _jobType.equals(other._jobType);
        }
        return false;
    } 

    @Override
    public Office clone() {
        Office r = (Office) super.clone();
        r._jobType = new String(_jobType);
        return r;
    }
}