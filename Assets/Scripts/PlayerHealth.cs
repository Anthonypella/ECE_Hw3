using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.SceneManagement;

public class PlayerHealth : MonoBehaviour
{
    public int maxHealth;
    public int maxMana;
   
    private int currentHealth;
    private int currentMana;

    public Image healthBar;
    public Image manaBar;

    public float manaRegen = .1f;
    float timer = 0;

    // Start is called before the first frame update
    void Start()
    {
        currentHealth = maxHealth;
        currentMana = maxMana;

    }
    private void Update()
    {
        timer += Time.deltaTime;
        if(timer >= manaRegen)
        {
            timer = 0;
            currentMana = Mathf.Min(currentMana + 1, maxMana);
            updateHealthAndMana();
        }
    }
    public bool spendMana(int amt)
    {
        if(amt <= currentMana)
        {
            currentMana -= amt;
            updateHealthAndMana();
            return true;
        }
        else
        {
            return false;
        }
    }
    public void takeDamage(int damage, bool damageEnemies)
    {
        Debug.Log(damage);
        currentHealth -= damage;
        updateHealthAndMana();
        if(currentHealth <= 0)
        {
            die(0);
        }
        
            

    }
   public void updateHealthAndMana()
    {
        healthBar.fillAmount = (float)((float)currentHealth / (float)maxHealth);
        manaBar.fillAmount = Mathf.Lerp(manaBar.fillAmount, (float)((float)currentMana / (float)maxMana),.25f);
    }
    public void die(int scene)
    {

        SceneManager.LoadScene(scene);
    }
}
