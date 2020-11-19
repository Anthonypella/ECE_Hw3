using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.AI;

public class playerMotor : MonoBehaviour
{
    private NavMeshAgent agent;
    private Transform target;
    private bool isLooking = false;
    public float dashTime;
    public float timer = 0;
    private bool isDashing = false;
    public float turnSpeed = .1f;
    bool isSlowed = false;
   
    // Start is called before the first frame update
    void Start()
    {
        agent = transform.GetComponent<NavMeshAgent>();
        agent.SetDestination(transform.position + transform.forward * 5);
    }
    private void Update()
    {
        if (target && isLooking)
        {
            Quaternion lookAt = Quaternion.LookRotation(target.position - transform.position);
            Quaternion lerp = Quaternion.Lerp(transform.rotation, lookAt, turnSpeed);
            transform.rotation = lerp;
        }
    }
    public void setLoc(Vector3 p)
    {
        agent.SetDestination(p);
        
    }
    public void setLocation(Vector3 point)
    {
        agent.SetDestination(point);
        
        agent.isStopped = false;
        agent.updateRotation = true;
        agent.SetDestination(point);

    }
    public bool isInAttackRange()
    {
        if (agent.stoppingDistance > Vector3.Distance(transform.position, target.position))
        {

            return true;

        }
        else
        {
            return false;
        }
    }
    public IEnumerator slowDown(float slowAmt, float time)
    {
        if (!isSlowed)
        {
            agent.speed *= slowAmt;
            agent.velocity *= slowAmt;
            isSlowed = true;
            yield return new WaitForSeconds(time);
            agent.speed /= slowAmt;
            isSlowed = false;
        }
        
    }
    public void stop()
    {       
        agent.isStopped = true;
        agent.updateRotation = false;
    }
    public void lookAt(Transform pos)
    {
        transform.LookAt(pos);
        stop();
    }
    public void setTarget(Transform t)
    {
        target = t;
        isLooking = true;
        stop();
    }
    public void nullTarget()
    {
        target = null;
        isLooking = false;
        agent.isStopped = false;
        agent.updateRotation = true;
    }

    // Update is called once per frame
   
}
